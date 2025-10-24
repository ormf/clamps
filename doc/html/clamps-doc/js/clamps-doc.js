function calculateSettingAsThemeString({ localStorageTheme, systemSettingDark }) {
  if (localStorageTheme !== null) {
    return localStorageTheme;
  }

  if (systemSettingDark.matches) {
    return "dark";
  }
  return "light";
}

var r = document.querySelector(':root');
var tocHidden;
var tocUnfolded;

function toggleClampsTocUnfold () {
    if (tocUnfolded == 'true') {
        r.style.setProperty('--clamps-toc-hidden-display', 'none');
        tocUnfolded = 'false';
    }
    else {
        r.style.setProperty('--clamps-toc-hidden-display', 'display-block');
        tocUnfolded = 'true';
    }
    localStorage.setItem('clamps-toc-unfolded', tocUnfolded);
}

function setTocStartState () {
    let localStorageTocUnfolded = localStorage.getItem("clamps-toc-unfolded");
    let localStorageTocHidden = localStorage.getItem("clamps-toc-hidden");
    
    if (localStorageTocUnfolded != null && localStorageTocUnfolded == "false") {
        console.log("toc-unfolded = false");
        tocUnfolded = "true";
    }
    else {
        tocUnfolded = "false";
        console.log("toc-unfolded = true");
    }
    if (localStorageTocHidden != null && localStorageTocHidden == "false") {
        console.log("toc-start: hidden = false");
        tocHidden = "true";
    }
    else {
        tocHidden = "false";
        console.log("toc-start: hidden = true");
    }
    toggleToc();
    toggleClampsTocUnfold();
}

const localStorageTheme = localStorage.getItem("clamps-doc-theme");

const systemSettingDark = window.matchMedia("(prefers-color-scheme: dark)");

var currentThemeSetting = calculateSettingAsThemeString({ localStorageTheme, systemSettingDark });

function toggleTheme () {

    if (currentThemeSetting == "light")
        currentThemeSetting = "dark";
    else
        currentThemeSetting = "light";
    localStorage.setItem("clamps-doc-theme", currentThemeSetting);
    document.querySelector("html").setAttribute("data-theme", currentThemeSetting);
}

function toggleToc () {
    let rs = getComputedStyle(r);
    if (tocHidden == "false") {
        // document.getElementById("table-of-contents").hidden = true;
        console.log('setting toc hidden');
        tocHidden = "true";
        document.getElementById("table-of-contents").classList.remove('toc-open');
        document.getElementById("table-of-contents").classList.add('toc-closed');
    }
    else {
        tocHidden = "false";
        console.log('setting toc visible');
        document.getElementById("table-of-contents").classList.remove('toc-closed');
        document.getElementById("table-of-contents").classList.add('toc-open');
    }
    localStorage.setItem("clamps-toc-hidden", tocHidden);
}

function init() {
    let mainBody = document.querySelector('body');
    const topButtons = document.createElement('div');
    const tocButton = document.createElement('button');
    const themeButton = document.createElement('button');
//    console.log(navLeft);
    console.log('init clamps-doc.js');

    topButtons.setAttribute('id', 'top-buttons');
    tocButton.setAttribute('id', 'toc-button');
    themeButton.setAttribute('id', 'theme-button');
    mainBody.insertBefore(topButtons, mainBody.firstChild);
    topButtons.appendChild(tocButton);
    topButtons.appendChild(themeButton);

    tocButton.addEventListener('click', () => {
        //      alert('New button clicked!');
        toggleToc();
    });
    themeButton.addEventListener('click', () => {
        //      alert('New button clicked!');
        toggleTheme();
    });
    setTocStartState();
    document.querySelector("html").setAttribute("data-theme", currentThemeSetting);
    localStorage.setItem("theme", currentThemeSetting);
    let activeToc = document.getElementsByClassName("toc-active")[0];
    activeToc.scrollIntoView({behavior: 'auto', block: 'center'});
    document.getElementById("table-of-contents").scrollLeft = 0;
    document.body.scrollIntoView();
}

document.querySelector("html").setAttribute("data-theme", currentThemeSetting);
localStorage.setItem("theme", currentThemeSetting);
window.onload = (event) => { init(); }
