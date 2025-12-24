function googleTranslateElementInit() {
    try {
        if (typeof google !== 'undefined' &&
            google.translate &&
            typeof google.translate.TranslateElement === 'function') {
            new google.translate.TranslateElement({
                pageLanguage: "en",
                includedLanguages: "af,sq,am,ar,hy,az,eu,be,bn,bs,bg,ca,ceb,zh-CN,zh-TW,co,hr,cs,da,nl,en,eo,et,fil,fi,fr,fy,gl,ka,de,el,gu,ht,ha,haw,iw,hi,hmn,hu,is,ig,id,ga,it,ja,jw,kn,kk,km,rw,ko,ku,ky,lo,la,lv,lt,lb,mk,mg,ms,ml,mt,mi,mr,mn,my,ne,no,ps,fa,pl,pt,pa,ro,ru,sm,gd,sr,st,sn,sd,si,sk,sl,so,es,su,sw,sv,tg,ta,tt,te,th,tr,tk,uk,ur,ug,uz,vi,cy,xh,yi,yo,zu"
            }, "google_translate_element");
        }
    } catch (e) {
        console.log("Google Translate initialization skipped:", e.message);
    }
}

window.toggleTranslate = function() {
    var translateElement = document.getElementById('google_translate_element');

    if (!translateElement) {
        return;
    }

    var gadget = translateElement.querySelector('.goog-te-gadget');
    var combo = translateElement.querySelector('.goog-te-combo');
    var select = translateElement.querySelector('select');

    if (!gadget && !combo && !select) {
        alert('Language selector is loading. Please try again in a moment.');
        return;
    }

    if (translateElement.classList.contains('show')) {
        translateElement.classList.remove('show');
        translateElement.style.display = 'none';
        translateElement.style.visibility = 'hidden';
    } else {
        translateElement.classList.add('show');
        translateElement.style.display = 'inline-block';
        translateElement.style.visibility = 'visible';

        if (gadget) {
            gadget.style.display = 'inline-block';
            gadget.style.visibility = 'visible';
        }
        if (combo) {
            combo.style.display = 'inline-block';
            combo.style.visibility = 'visible';
        }
        if (select) {
            select.style.display = 'inline-block';
            select.style.visibility = 'visible';
        }
    }
}

var ttsState = {
    synth: window.speechSynthesis,
    utterance: null,
    isActive: false,
    isPaused: false,
    voices: []
};

function loadVoices() {
    ttsState.voices = ttsState.synth.getVoices();
    if (ttsState.voices.length === 0) {
        ttsState.synth.addEventListener('voiceschanged', function() {
            ttsState.voices = ttsState.synth.getVoices();
        });
    }
}

function detectCurrentLanguage() {
    var translateElement = document.getElementById('google_translate_element');
    if (translateElement) {
        var select = translateElement.querySelector('select');
        if (select && select.value) {
            return select.value;
        }
    }
    return 'en';
}

function selectBestVoice(language) {
    if (ttsState.voices.length === 0) {
        loadVoices();
        return null;
    }

    var langCode = language.split('-')[0];
    var availableVoices = ttsState.voices.filter(function(voice) {
        return voice.lang.toLowerCase().startsWith(langCode.toLowerCase());
    });

    if (availableVoices.length === 0) {
        availableVoices = ttsState.voices.filter(function(voice) {
            return voice.lang.toLowerCase().startsWith('en');
        });
    }

    if (availableVoices.length === 0) {
        return null;
    }

    var femaleVoices = [];
    var maleVoices = [];

    availableVoices.forEach(function(voice) {
        var name = voice.name.toLowerCase();
        var uri = (voice.voiceURI || '').toLowerCase();
        var combined = name + ' ' + uri;

        var femalePatterns = /female|woman|zira|hazel|susan|linda|heather|samantha|victoria|karen|moira|fiona|amélie|audrey|marie|monica|paulina|carmit|kyoko|sin-ji|ting-ting|ya-ling|yuna/i;
        var malePatterns = /\b(male|man|david|mark|richard|alex|daniel|thomas|fred|oliver|diego|jorge|juan|nicolas|otoya|luca|xander|henrik)\b/i;

        if (combined.match(femalePatterns)) {
            femaleVoices.push(voice);
        } else if (combined.match(malePatterns) && !combined.match(/female/i)) {
            maleVoices.push(voice);
        }
    });

    if (femaleVoices.length > 0) {
        return femaleVoices[0];
    }

    if (maleVoices.length > 0) {
        return maleVoices[0];
    }

    return availableVoices[0];
}

window.toggleTTS = function() {
    if (!ttsState.synth) {
        alert('Text-to-speech is not supported in your browser. Please use Chrome, Firefox, Safari, or Edge.');
        return;
    }

    if (ttsState.isActive) {
        stopTTS();
    } else {
        startTTS();
    }
}

function startTTS() {
    if (ttsState.synth.speaking) {
        ttsState.synth.cancel();
    }

    loadVoices();

    var textToRead = '';

    if (window.getSelection && window.getSelection().toString()) {
        textToRead = window.getSelection().toString();
    } else {
        var mainContent = document.getElementById('main-content');
        if (mainContent) {
            textToRead = mainContent.innerText || mainContent.textContent;
        }
    }

    textToRead = textToRead.trim();

    if (!textToRead) {
        alert('No text found to read.');
        return;
    }

    if (textToRead.length > 5000) {
        textToRead = textToRead.substring(0, 5000);
    }

    ttsState.utterance = new SpeechSynthesisUtterance(textToRead);

    var currentLang = detectCurrentLanguage();
    ttsState.utterance.lang = currentLang;

    var selectedVoice = selectBestVoice(currentLang);
    if (selectedVoice) {
        ttsState.utterance.voice = selectedVoice;
    }

    ttsState.utterance.onstart = function() {
        ttsState.isActive = true;
        ttsState.isPaused = false;
        updateTTSIcon();
    };

    ttsState.utterance.onend = function() {
        ttsState.isActive = false;
        ttsState.isPaused = false;
        updateTTSIcon();
    };

    ttsState.utterance.onerror = function() {
        ttsState.isActive = false;
        ttsState.isPaused = false;
        updateTTSIcon();
    };

    ttsState.synth.speak(ttsState.utterance);
}

function pauseTTS() {
    if (ttsState.synth && ttsState.isActive) {
        if (ttsState.isPaused) {
            ttsState.synth.resume();
            ttsState.isPaused = false;
            announceToScreenReader('Text to speech resumed');
        } else {
            ttsState.synth.pause();
            ttsState.isPaused = true;
            announceToScreenReader('Text to speech paused');
        }
        updateTTSIcon();
    }
}

function stopTTS() {
    if (ttsState.synth) {
        ttsState.synth.cancel();
        ttsState.isActive = false;
        ttsState.isPaused = false;
        updateTTSIcon();
        announceToScreenReader('Text to speech stopped');
    }
}

function updateTTSIcon() {
    var ttsToggle = document.getElementById('tts_toggle');
    var ttsIcon = document.getElementById('tts_icon');

    if (!ttsToggle || !ttsIcon) return;

    if (ttsState.isActive) {
        ttsToggle.classList.add('tts-active');
        if (ttsState.isPaused) {
            ttsIcon.className = 'fa fa-pause';
            ttsToggle.title = 'Resume Speech (Alt+S)';
        } else {
            ttsIcon.className = 'fa fa-volume-up';
            ttsToggle.title = 'Stop Speech (Alt+S)';
        }
    } else {
        ttsToggle.classList.remove('tts-active');
        ttsIcon.className = 'fa fa-volume-up';
        ttsToggle.title = 'Text to Speech (Alt+S)';
    }
}

function announceToScreenReader(message) {
    var ariaRegion = document.getElementById('accessible-notifications');
    if (ariaRegion) {
        ariaRegion.textContent = message;
    }
}

document.addEventListener('keydown', function(e) {
    if (e.altKey && e.key === 's') {
        e.preventDefault();
        toggleTTS();
    }
    if (e.altKey && e.key === 'p') {
        e.preventDefault();
        pauseTTS();
    }
    if (e.key === 'Escape' && ttsState.isActive) {
        e.preventDefault();
        stopTTS();
    }
});

$(document).ready(function() {
    function fixTooltipWrapping() {
        $('.hoverlayer .hovertext').each(function() {
            var $this = $(this);
            var $rect = $this.find('rect');
            var $text = $this.find('text');

            $text.attr('text-anchor', 'start');
            $text.css('text-align', 'left');

            var textWidth = $text[0] ? $text[0].getBBox().width : 0;
            if (textWidth > 400) {
                $rect.attr('width', 450);
                $text.find('tspan').each(function() {
                    var tspan = $(this);
                    var text = tspan.text();
                    if (text.length > 60) {
                        tspan.attr('style', 'word-wrap: break-word; white-space: pre-wrap;');
                    }
                });
            }
        });
    }

    var observer = new MutationObserver(function(mutations) {
        fixTooltipWrapping();
    });

    observer.observe(document.body, {
        childList: true,
        subtree: true,
        attributes: true,
        attributeFilter: ['class']
    });

    $(document).on('plotly_hover', function() {
        setTimeout(function() {
            fixTooltipWrapping();
        }, 10);
    });

    if (!$('#accessible-notifications').length) {
        $('body').append('<div id="accessible-notifications" class="sr-only" role="status" aria-live="polite" aria-atomic="true"></div>');
    }
    if (!$('#visual-notifications').length) {
        $('body').append('<div id="visual-notifications" class="notification-container" role="region" aria-label="Notifications"></div>');
    }

    var observer = new MutationObserver(function(mutations) {
        mutations.forEach(function(mutation) {
            if (mutation.type === 'childList') {
                mutation.addedNodes.forEach(function(node) {
                    if (node.nodeType === 1) {
                        var ariaLabel = node.getAttribute('aria-label');
                        if (ariaLabel) {
                            var ariaRegion = document.getElementById('accessible-notifications');
                            if (ariaRegion) {
                                ariaRegion.textContent = ariaLabel;
                            }
                        }
                    }
                });
            }
        });
    });

    var visualNotifications = document.getElementById('visual-notifications');
    if (visualNotifications) {
        observer.observe(visualNotifications, { childList: true });
    }

    var translateIcon = $('#translate_icon');
    if (translateIcon.length > 0) {
        translateIcon.on('click touchend', function(e) {
            e.preventDefault();
            e.stopPropagation();
            window.toggleTranslate();
            return false;
        });
    }

    var ttsToggle = $('#tts_toggle');
    if (ttsToggle.length > 0) {
        ttsToggle.on('click touchend', function(e) {
            e.preventDefault();
            e.stopPropagation();
            window.toggleTTS();
            return false;
        });
    }

    loadVoices();
    updateTTSIcon();
});

Shiny.addCustomMessageHandler('accessibleNotification', function(message) {
    const {text, type = 'info', duration = null, id = null, dismissible = true, priority = 'polite'} = message;
    const typeConfig = {
        'error': {role: 'alert', icon: '⚠️', ariaLabel: 'Error: ', class: 'notification-error'},
        'warning': {role: 'alert', icon: '⚡', ariaLabel: 'Warning: ', class: 'notification-warning'},
        'success': {role: 'status', icon: '✓', ariaLabel: 'Success: ', class: 'notification-success'},
        'info': {role: 'status', icon: 'ℹ️', ariaLabel: 'Information: ', class: 'notification-info'},
        'progress': {role: 'status', icon: '⏳', ariaLabel: 'Processing: ', class: 'notification-progress'}
    };
    const config = typeConfig[type] || typeConfig['info'];
    const notificationId = id || 'notification-' + Date.now();

    $('#accessible-notifications').attr('aria-live', priority).text(config.ariaLabel + text);

    const visualNotification = $('<div id="' + notificationId + '" class="notification ' + config.class + '" role="' + config.role + '" tabindex="-1" aria-label="' + config.ariaLabel + text + '"><div class="notification-content"><span class="notification-icon" aria-hidden="true">' + config.icon + '</span><span class="notification-text">' + text + '</span>' + (dismissible ? '<button class="notification-close" aria-label="Dismiss notification" title="Dismiss (Esc key)"><span aria-hidden="true">×</span></button>' : '') + '</div>' + (type === 'progress' ? '<div class="notification-progress-bar" role="progressbar" aria-label="Loading"></div>' : '') + '</div>');

    if (id) $('#' + id).remove();
    $('#visual-notifications').append(visualNotification);

    if (type === 'error' || priority === 'assertive') visualNotification.focus();

    if (dismissible) {
        visualNotification.find('.notification-close').on('click', function() {
            $('#' + notificationId).attr('aria-hidden', 'true').fadeOut(300, function() { $(this).remove(); });
        });
        visualNotification.on('keydown', function(e) {
            if (e.key === 'Escape' || e.keyCode === 27) {
                $('#' + notificationId).attr('aria-hidden', 'true').fadeOut(300, function() { $(this).remove(); });
            }
        });
    }

    if (duration !== null) {
        const minDuration = 5000 + (text.length / 20) * 1000;
        const actualDuration = Math.max(duration, minDuration);
        setTimeout(function() {
            $('#' + notificationId).attr('aria-hidden', 'true').fadeOut(300, function() { $(this).remove(); });
        }, actualDuration);
    }
});

Shiny.addCustomMessageHandler('dismissNotification', function(id) {
    $('#' + id).attr('aria-hidden', 'true').fadeOut(300, function() { $(this).remove(); });
});

// Immediate conditional panel updates for dataset selection
$(document).ready(function() {
  // Remove any delay in conditional panel evaluation
  if (typeof Shiny !== 'undefined') {
    // Force immediate update on dataset_choice change
    $(document).on('change', '#dataset_choice', function() {
      // Trigger immediate conditional panel re-evaluation
      Shiny.shinyapp.$updateConditionals();
    });

    // Also handle selectize events
    $(document).on('change', '.selectize-input', function() {
      Shiny.shinyapp.$updateConditionals();
    });
  }
});

// Shiny notification close button handler
$(document).on('shiny:notification', function(event) {
  event.notification.closeButton = true;
});

$(document).on('mouseover', '.shiny-notification', function(e) {
  e.stopPropagation();
});

// Coding mode custom message handler - wait for Shiny to be available
$(document).on('shiny:connected', function() {
  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('setCodingMode', function(mode) {
      Shiny.setInputValue('coding_mode', mode);
    });
  }
});

// Topic modeling path tab visibility handler
$(document).on('shiny:inputchanged', function(event) {
  if (event.name === 'topic_modeling_path') {
    var path = event.value;
    var tabsToHide = ['8', '9', '10'];
    tabsToHide.forEach(function(tabValue) {
      var tabLink = $('#conditioned3 a[data-value="' + tabValue + '"]');
      if (tabLink.length > 0) {
        if (path === 'embedding') {
          tabLink.parent().hide();
        } else {
          tabLink.parent().show();
        }
      }
    });
  }
});

// Dark mode toggle function
function toggleDarkMode() {
  const html = document.documentElement;
  const currentTheme = html.getAttribute('data-theme');
  const newTheme = currentTheme === 'dark' ? 'light' : 'dark';
  html.setAttribute('data-theme', newTheme);
  localStorage.setItem('theme', newTheme);

  const btn = document.getElementById('dark_mode_toggle');
  if (btn) {
    const icon = btn.querySelector('i');
    if (icon) {
      icon.className = newTheme === 'dark' ? 'fa fa-sun' : 'fa fa-moon';
    }
    btn.setAttribute('aria-label', newTheme === 'dark' ? 'Switch to light mode' : 'Switch to dark mode');
    btn.title = newTheme === 'dark' ? 'Switch to light mode' : 'Switch to dark mode';
  }
}

// Initialize theme on page load
$(document).ready(function() {
  const savedTheme = localStorage.getItem('theme') || 'light';
  document.documentElement.setAttribute('data-theme', savedTheme);

  const btn = document.getElementById('dark_mode_toggle');
  if (btn) {
    const icon = btn.querySelector('i');
    if (icon) {
      icon.className = savedTheme === 'dark' ? 'fa fa-sun' : 'fa fa-moon';
    }
    btn.setAttribute('aria-label', savedTheme === 'dark' ? 'Switch to light mode' : 'Switch to dark mode');
    btn.title = savedTheme === 'dark' ? 'Switch to light mode' : 'Switch to dark mode';
  }

  // Initialize Google Translate
  var initAttempts = 0;
  var maxAttempts = 10;

  function tryInitTranslate() {
    initAttempts++;
    var translateElement = document.getElementById('google_translate_element');

    if (!translateElement) {
      if (initAttempts < maxAttempts) {
        setTimeout(tryInitTranslate, 500);
      }
      return;
    }

    if (typeof google !== 'undefined' &&
        google.translate &&
        typeof google.translate.TranslateElement === 'function' &&
        typeof googleTranslateElementInit === 'function') {
      if (translateElement.innerHTML.trim() === '') {
        googleTranslateElementInit();
        setTimeout(function() {
          translateElement.style.display = 'none';
        }, 100);
      } else {
        translateElement.style.display = 'none';
      }
    } else {
      if (initAttempts < maxAttempts) {
        setTimeout(tryInitTranslate, 500);
      }
    }
  }

  setTimeout(tryInitTranslate, 500);

  // Activate first tab by default
  $('#home_nav_menu li:first-child a').tab('show');
});

// Ensure only one option is highlighted at a time in selectize
$(document).ready(function() {
  $(document).on('mouseenter', '.selectize-dropdown .option', function() {
    // Remove active class from all options
    $('.selectize-dropdown .option').removeClass('active');
    // Add active class only to the current hovered option
    $(this).addClass('active');
  });
  
  $(document).on('mouseleave', '.selectize-dropdown', function() {
    // Clear all active states when mouse leaves dropdown
    $('.selectize-dropdown .option').removeClass('active');
  });
});

