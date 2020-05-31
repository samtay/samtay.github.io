//Theme initialize
if (!localStorage.getItem('theme'))
  localStorage.setItem('theme', "light");
themer();

// Theme toggle
function toggle() {
  if(localStorage.getItem('theme') == "light")
    localStorage.setItem('theme', "dark");
  else if(localStorage.getItem('theme') == "dark")
    localStorage.setItem('theme', "light");
  themer();
}

// Theme set
function themer() {
  var tone = localStorage.getItem('theme');
  var light = document.getElementById("light");
  var utterances = document.querySelector('iframe');

  if (utterances) {
    utterances.contentWindow.postMessage(
      {type: 'set-theme', theme: 'github-'+tone},
      'https://utteranc.es'
    );
  }

  dark.disabled = (tone == "light");
}
;
