;;; 
;;; ats-track-load-save.lisp
;;;
;;; redefinitions of ats-cuda functions in the context of the clamps
;;; ecosystem (using paths for loading soundfiles and ats files,
;;; returning ats structs when tracking and loading, etc.)
;;;
;;; **********************************************************************
;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :ats-cuda)

;;; (import '(cl-user:*sfile-path*) 'ats-cuda)

(shadowing-import '(cl-user:*sfile-path* cl-user:*ats-file-path*) 'ats-cuda)
(shadowing-import '(ou:path-find-file) 'ats-cuda)


(pushnew (asdf:system-relative-pathname :ats-cuda "snd/") cl-user:*sfile-path*)

(defun track-ats (file &rest args
                  &key
                    (start 0.0)
                    (duration nil)
                    (lowest-frequency 20)
                    (highest-frequency 20000.0)
                    (frequency-deviation 0.1)
                    (window-cycles 4)
                    (window-type 'blackman-harris-4-1)
                    (hop-size 1/4)
                    (fft-size nil)
                    (lowest-magnitude (db-amp -60))
                    (track-length 3)
                    (min-segment-length 3)
                    (last-peak-contribution 0.0)
                    (SMR-continuity 0.0)
;;;             (SMR-threshold nil) ;;; unused!
                    (amp-threshold nil)
                    (residual nil)
                    (par-energy t)
                    (optimize t)
                    (debug nil)
                    (verbose nil)
                    (force-M NIL)
                    (force-window NIL)
                    )
  "Analyze a soundfile /file/ and return its <<ats-sound>> structure. The
soundfile will be searched in all paths contained in <<*sfile-path*>> and
its subdirectories.

@Arguments
file - String denoting filename of the soundfile within *ats-snd-dir* to analyze.
:start - Number indicating time offset into the soundfile in seconds.
:duration - Number indicating the duration to analyze or nil for end of soundfile.
:lowest-frequency - Number indicating the lowest frequency to capture (determines Fourier Window width)
:highest-frequency - Number indicating the highest frequency to capture.
:frequency deviation - Number indicating the maximum frequency deviation of successive frames to be considered for the same track.
:window-cycles - Positive Integer denoting the overlap factor of successive analysis windows.
:window-type - The amplitude envelope type to use for each analysis window.
:hop-size - Positive Integer denoting the hop-size of successive analysis windows.
:fft-size - Positive Integer denoting the FFT size or nil, denoting to calculate the FFT size based on the lowest frequency.
:lowest-magnitude - Positive Integer in the range [0..1] setting the threshold for analysis peaks to be considered.
:track-length - Positive Integer.
:min-segment-length - Positive Integer indicating the minimum number of successive segments within the frequency deviation to qualify for a track.
:last-peak-contribution - Number.
:SMR-continuity - Number.
:amp-threshold - Boolean.
:residual - String denoting the filename for the residual noise soundfile or nil if residual noise shouldn't get calculated.
:par-energy - Boolean indicating whether to distribute the residual noise to the partials.
:optimize - Boolean indicating whether to optimize the results.
:debug - Boolean flag to show debug info in the REPL.
:verbose - Boolean flag for verbose output.
:force-M - Boolean.
:force-window - Boolean.
@See-also
clamps:ats-cuda-display
ats-sound
load-ats
save-ats
"
  (let ((filepath (path-find-file file cl-user:*sfile-path*)))
    (if filepath
        (let* (;;; input file
               (fname (namestring filepath))
               (fil (open-input* fname :restartable t))
;;; ATS sound
               (sound (make-ats-sound :name (file-namestring filepath)
                                      :analysis-params args))
;;; file sampling-rate
               (file-sampling-rate (sound-srate fil))   
;;; index of first sample to read
               (st (floor (* start file-sampling-rate)))
;;; index of last sample to read
               (nd (if duration
                       (+ st (floor (* duration file-sampling-rate)))
                       (sound-framples fil)))
;;; number of samples to read
               (total-samps (- nd st))
               (input-data (sfile->array fname :count total-samps))
;;; file duration
               (file-duration (double (/ total-samps file-sampling-rate)))
;;; number of samples in a cycle
               (cycle-samps (floor
                             (* (/ 1 lowest-frequency)
                                window-cycles file-sampling-rate)))
;;; we want an odd lengthed window centered at time 0.0
               (M (if force-M force-M
                      (if (evenp cycle-samps)
                          (1+ cycle-samps)
                          cycle-samps)))
;;; fft size is next power of 2 or forced by user
               (N (if fft-size
                      fft-size
                      (ppp2 (* 2 M))))
;;; fft structure
               (fft-struct
                 (make-ats-fft
                  :size N
                  :rate file-sampling-rate
                  :fdr (make-double-float-array N :initial-element (double 0.0))
                  :fdi (make-double-float-array N :initial-element (double 0.0))))
;;; window array
               (window (if force-window force-window
                           (if (symbolp window-type)
                               (make-blackman-window window-type M)
                               (make-fft-window window-type M))))
;;; window normalization
               (norm (window-norm window))
;;; hop in samples
               (hop (floor (* M hop-size))) 
;;; number of analysis frames
               (frames (compute-frames total-samps hop st nd))
;;; we keep sample numbers of central points of the windows
               (win-samps (make-array frames :initial-element 0))
;;; magic number for fft frequencies (frquency resolution)
               (fft-mag (double (/ file-sampling-rate N)))
;;; lowest frequency to analyze
               (l-Frq (if (>= lowest-frequency 0.0)
                          lowest-frequency 0.0))
;;; highest frequency to analyze
               (h-Frq (if (<= highest-frequency (/ file-sampling-rate 2)) 
                          highest-frequency 
                          (floor file-sampling-rate 2)))
;;; lowest bin to read 
               (lowest-bin (floor l-Frq fft-mag))
;;; highest bin to read 
               (highest-bin (floor h-Frq fft-mag))
;;; Arrays for data
;;; array of lists for peaks
               (ana-frames (make-array frames :element-type 'list :initial-element nil))
;;; various vars
;;; timer
               (tmp 0.0)
               (smp 0)
;;; central point of the window
               (M-over-2 (floor (- M 1) 2))
;;; first point in fft buffer where to write
               (first-point (- N M-over-2))
;;; set file pointer half a window from the first sample
               (filptr (- st M-over-2))
;;; minimum SMR (unused?)
;;;  (min-smr (if SMR-threshold SMR-threshold 0.0))
               (n-partials 0)
               (tracks nil)
               (peaks nil)
               (unmatched-peaks nil))
;;;    (break "total-samps: ~a" total-samps)
;;; tell user we start tracking partials
          (format t "~&total-samps: ~a~%" total-samps)
          (format t "~&cycle-samps: ~a~%" cycle-samps)
          (format t "~&frames= ~D " frames)
          (format t "M = ~D N = ~D~%" M N)
          (format t "~&Tracking...~%")
;;; Main loop
          (loop
            for frame-n from 0 below frames
            with modulo = (floor frames 40)
            do
               (when (zerop (mod frame-n modulo))
                 (format t "."))
;;; clear fft arrays
               (clear-array (ats-fft-fdr fft-struct))
               (clear-array (ats-fft-fdi fft-struct))
;;; multiply by window
               (loop for k from 0 below M do
                 (if (>= filptr 0) 
                     (setf (aref (ats-fft-fdr fft-struct) (mod (+ k first-point) N)) 
                           (* (aref window k)
                              (if (< filptr total-samps)
                                  (aref input-data filptr)
                                  (double 0.0)))))
                 (incf filptr))
;;;      (format t "~a..." frame-n)
;;; note that after the loop filptr=M and not M-1
;;; the sample at the middle of the window is:
               (setf smp (- filptr M-over-2 1))
               (if debug (format t "smp=~d, frame-n=~d " smp frame-n))
;;; we keep sample numbers of window midpoints in an array
               (setf (aref win-samps frame-n) smp)
;;; set timer
               (setf tmp (double (/ (- smp st) file-sampling-rate)))
;;; get the dft 
               (fft
                (ats-fft-fdr fft-struct)
                (ats-fft-fdi fft-struct)
                (ats-fft-size fft-struct)
                1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Peak Detection:
;;; get peaks (amplitudes normalized by window norm)
;;; list of peaks is sorted by frequency
               (setf peaks (peak-detection fft-struct 
                                           :lowest-bin lowest-bin 
                                           :highest-bin highest-bin 
                                           :lowest-magnitude lowest-magnitude 
                                           :norm norm))
;;; process peaks
               (when peaks 
;;; evaluate masking values (SMR) of peaks
                 (evaluate-smr peaks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Peak Tracking:
;;; try to match peaks
;;; only if we have at least 2 frames
;;; and if we have active tracks 
                 (if (and (> frame-n 0) 
                          (setf tracks (update-tracks tracks track-length frame-n ana-frames last-peak-contribution)))
                     (progn
;;; track peaks and get leftover
                       (setf unmatched-peaks (peak-tracking (sort (copy-seq tracks) #'> :key #'ats-peak-smr) 
                                                            peaks frequency-deviation SMR-continuity))
;;; kill unmatched peaks from previous frame
                       (dolist (k (first unmatched-peaks))
;;; we copy the peak into this frame but with amp 0.0 
;;; this represents our death trajectory
                         (push
                          (make-ats-peak
                           :amp 0.0 :smr 0.0
                           :pha (ats-peak-pha k)
                           :frq (ats-peak-frq k)
                           :track (ats-peak-track k)) peaks))
;;; give birth to peaks from new frame
                       (dolist (k (second unmatched-peaks))
;;; set track number of unmatched peaks
                         (setf (ats-peak-track k) n-partials)
                         (incf n-partials)
;;; we copy the peak into the previous frame but with amp 0.0 
;;; this represents our born trajectory
                         (push
                          (make-ats-peak
                           :amp 0.0 :smr 0.0
                           :pha (ats-peak-pha k)
                           :frq (ats-peak-frq k)
                           :track (ats-peak-track k))
                          (aref ana-frames (1- frame-n)))
                         (push (ats-cuda::copy-ats-peak k) tracks)))
;;; give number to all peaks
                     (dolist (k (sort (copy-seq peaks) #'< :key #'ats-peak-frq))
                       (setf (ats-peak-track k) n-partials)
                       (incf n-partials)))
                 (setf (aref ana-frames frame-n) peaks))
;;; update file pointer
               (setf filptr (+ (- filptr M) hop))
               (if verbose (format t "<Frame:~d Time:~4,3F Tracks:~4,3F> " frame-n tmp n-partials)))
          (format t "~%")
;;; Initialize ATS sound
          (ats-cuda::init-sound sound 
                      :sampling-rate file-sampling-rate
                      :frame-size hop
                      :window-size M
                      :frames frames 
                      :duration file-duration 
                      :partials n-partials)
;;; and fill it up with data
          (loop for k from 0 below n-partials do
            (loop for frame from 0 below frames do
              (let ((pe (find k (aref ana-frames frame) :key #'ats-peak-track)))
                (if pe
                    (setf (aref (ats-sound-amp sound) k frame)(double (ats-peak-amp pe))
                          (aref (ats-sound-frq sound) k frame)(double (ats-peak-frq pe))
                          (aref (ats-sound-pha sound) k frame)(double (ats-peak-pha pe))))
;;; set time anyways
                (setf (aref (ats-sound-time sound) k frame)
                      (double (/ (- (aref win-samps frame) st) file-sampling-rate))))))
;;; finally optimize and declare new sound in ATS
          (if optimize 
              (optimize-sound sound
                              :min-frq lowest-frequency 
                              :max-frq highest-frequency
                              :min-length (if min-segment-length
                                              min-segment-length
                                              *ats-min-segment-length*)
                              :amp-threshold (if amp-threshold
                                                 amp-threshold
                                                 *ats-amp-threshold*)
                              :verbose verbose))
          (if verbose (format t "Partials: ~d Frames: ~d~%" (ats-sound-partials sound)(ats-sound-frames sound)))
;;; fix freqs of 0.0d:
          (fix-freqs sound)
;;; register sound in the system
          (add-sound sound)
          (format t "~&Partials: ~d~%" (ats-sound-partials sound))
;;; now get the residual
          (when residual
            (compute-residual
             input-data residual sound win-samps file-sampling-rate :verbose verbose :srate file-sampling-rate)
            (residual-analysis residual sound :par-energy par-energy :verbose verbose :debug debug :equalize t))
          (close-input fil)
;;;    (ats-vectors->arrays sound)
          sound)
        (error "file ~S not found in *sfile-path*, aborting" file)
        )))

(defun load-ats (file &key (dist-energy T))
  "Load and return an ATS sound from /file/. Optionally move the energy
from the noise bands into the partials. /file/ will be searched
recursively in all directories of <<*ats-file-path*>>.

@Arguments
file - String denoting the filename of a soundfile.
:dist-energy - Boolean indicating whether to to transfer the noise from the boise-bands to the dist-energy in the partials.

@See-also
clamps:ats-cuda-display
*ats-file-path*
ats-sound
save-ats
track-ats
"
  (let* ((filepath (path-find-file file *ats-file-path*))
         (file (namestring filepath)))
;;; check if file exists
    (unless (probe-file file) (error "File ~s does not exist!" file))
    (with-open-file (in file :element-type '(unsigned-byte 8))
      (let* (
             (snd (make-ats-sound :name (file-namestring filepath)))
             (mag 0.0)
             (sr 0.0)
             (frame-size 0.0)
             (window-size 0.0)
             (partials 0.0)
             (frames 0.0)
             (max-amp 0.0)
             (max-frq 0.0)
             (dur 0.0)
             (type 0.0)
             (has-pha)
             (has-noi)
             (noi-arr)
             (data-arr)
             (header-arr (make-double-float-array *ats-header-size* :initial-element 0.0))
             (time-arr (make-double-float-array 1 :initial-element 0.0)))
;;; Read header information
        (clm-read-floats in header-arr *ats-header-size*)
;;; Check magic number
        (if (/= (aref header-arr 0) *ats-magic-number*)
            (error "Not a proper ATS file (may be byte swapped)~%"))
;;; Retrieve data from header
        (setf mag (aref header-arr 0)
              sr (floor (aref header-arr 1))
              frame-size (floor (aref header-arr 2))
              window-size (floor (aref header-arr 3))
              partials (floor (aref header-arr 4))
              frames (floor (aref header-arr 5))
              max-amp (aref header-arr 6)
              max-frq (aref header-arr 7)
              dur (aref header-arr 8)
              type (floor (aref header-arr 9)))
;;; Print Header out
        (format t "mag: ~S sr: ~S fs: ~S ws: ~S partials: ~S frames: ~S MaxAmp: ~S MaxFrq: ~S Dur: ~S Type: ~S~%"
                mag sr frame-size window-size partials frames max-amp max-frq dur type)
;;; set booleans
        (setf has-pha (if (evenp type) T NIL))
        (setf has-noi (if (> type 2) T NIL))
;;; create array for data
        (setf data-arr (make-double-float-array (if has-pha 3 2) :initial-element 0.0))
;;; create array for noise part if necessary
        (if has-noi (setf noi-arr (make-double-float-array *ats-critical-bands* :initial-element 0.0)))
        (format t "Loading sound...~%")
        (init-sound snd 
                    :sampling-rate sr
                    :frame-size frame-size
                    :window-size window-size
                    :frames frames
                    :partials partials
                    :duration dur
                    :has-phase has-pha
                    :has-noise has-noi)
        (setf (ats-sound-ampmax snd) max-amp)
        (setf (ats-sound-frqmax snd) max-frq)
        (loop for i from 0 below frames do
;;; Read frame's time
          (clm-read-floats in time-arr 1)
;;; Now loop reading each partial's data
          (loop for j from 0 below partials do
;;; set time
            (setf (aref (ats-sound-time snd) j i)
                  (aref time-arr 0))
;;; read data
            (clm-read-floats in data-arr (if has-pha 3 2))
;;; set amp
            (setf (aref (ats-sound-amp snd) j i)
                  (aref data-arr 0))
;;; set frq
            (setf (aref (ats-sound-frq snd) j i)
                  (aref data-arr 1))
;;; set pha
            (if has-pha
                (setf (aref (ats-sound-pha snd) j i)
                      (aref data-arr 2))))
                                        ;      (format t "[time: ~A amp: ~A frq: ~A pha: ~A] " 
                                        ;      (aref time-arr 0)(aref data-arr 0)(aref data-arr 1)(aref data-arr 2))
;;; finally set noise
          (when has-noi
            (clm-read-floats in noi-arr *ats-critical-bands*)
            (loop for k from 0 below *ats-critical-bands* do
              (setf (aref (ats-sound-band-energy snd) k i)(aref noi-arr k)))))

;;; optimize sound
        (optimize-load-sound snd :verbose t :get-max-values nil :fill-gaps nil :trim nil :simplify nil)
;;; distribute energy in partials in case we are asked to
        (when (and has-noi dist-energy)
          (format t "Transferring noise energy to partials...~%")
          (band-to-energy snd)
          (remove-bands snd))
;;; register new sound
        (pushnew (ats-sound-name snd) *ats-sounds* :test #'equal)
        snd))))

(setf (fdefinition 'save-ats) #'ats-cuda:ats-save)
