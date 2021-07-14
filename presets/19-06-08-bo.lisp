;;; 
;;; 19-06-08-bo.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2019 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :big-orchestra)

(defparameter *test*
  #(#S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))
    #S(preset
       :dtime #S(env :start 0.1 :delta 1 :attack 0 :release 0 :type :exp)
       :dtime-dev #S(env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp)
       :dur #S(env :start 3 :delta 1 :attack 0 :release 0 :type :exp)
       :dur-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :transp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :transp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :stretch #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :stretch-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize #S(env :start 123 :delta 1 :attack 0 :release 0 :type :exp)
       :wsize-dev #S(env :start 1 :delta 1 :attack 0 :release 0 :type :exp)
       :amp #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :amp-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin)
       :inner-attack-dev #S(env
                            :start 1
                            :delta 1
                            :attack 0
                            :release 0
                            :type :exp)
       :inner-release #S(env
                         :start 0.01
                         :delta 0
                         :attack 0
                         :release 0
                         :type :lin)
       :inner-release-dev #S(env
                             :start 1
                             :delta 1
                             :attack 0
                             :release 0
                             :type :exp)
       :chan 0
       :chan-dev #S(env :start 0 :delta 0 :attack 0 :release 0 :type :lin))))


