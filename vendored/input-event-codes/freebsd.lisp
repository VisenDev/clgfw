;;; freebsd.lisp - FreeBSD input event codes
;;;
;;; This is a port of all the constants defined on FreeBSD in
;;; </sys/dev/evdev/input-event-codes.h>. Documentation is preserved where
;;; applicable and only expressions are changed from C to Common Lisp syntax.
;;;
;;; Copyright (c) 2022 Samuel Hunter.
;;; This software is licensed under the MIT License.
;;; See LICENSE for details.

(defpackage #:xyz.shunter.input-event-codes
  (:nicknames #:input-event-codes)
  (:use #:cl)
  (:export
    +abs-brake+ +abs-cnt+ +abs-distance+ +abs-gas+ +abs-hat0x+ +abs-hat0y+
    +abs-hat1x+ +abs-hat1y+ +abs-hat2x+ +abs-hat2y+ +abs-hat3x+ +abs-hat3y+
    +abs-max+ +abs-misc+ +abs-mt-blob-id+ +abs-mt-distance+ +abs-mt-orientation+
    +abs-mt-position-x+ +abs-mt-position-y+ +abs-mt-pressure+ +abs-mt-slot+
    +abs-mt-tool-type+ +abs-mt-tool-x+ +abs-mt-tool-y+ +abs-mt-touch-major+
    +abs-mt-touch-minor+ +abs-mt-tracking-id+ +abs-mt-width-major+
    +abs-mt-width-minor+ +abs-pressure+ +abs-reserved+ +abs-rudder+ +abs-rx+
    +abs-ry+ +abs-rz+ +abs-throttle+ +abs-tilt-x+ +abs-tilt-y+ +abs-tool-width+
    +abs-volume+ +abs-wheel+ +abs-x+ +abs-y+ +abs-z+ +btn-0+ +btn-1+ +btn-2+
    +btn-3+ +btn-4+ +btn-5+ +btn-6+ +btn-7+ +btn-8+ +btn-9+ +btn-a+ +btn-b+
    +btn-back+ +btn-base+ +btn-base2+ +btn-base3+ +btn-base4+ +btn-base5+
    +btn-base6+ +btn-c+ +btn-dead+ +btn-digi+ +btn-dpad-down+ +btn-dpad-left+
    +btn-dpad-right+ +btn-dpad-up+ +btn-east+ +btn-extra+ +btn-forward+
    +btn-gamepad+ +btn-gear-down+ +btn-gear-up+ +btn-joystick+ +btn-left+
    +btn-middle+ +btn-misc+ +btn-mode+ +btn-mouse+ +btn-north+ +btn-pinkie+
    +btn-right+ +btn-select+ +btn-side+ +btn-south+ +btn-start+ +btn-stylus+
    +btn-stylus2+ +btn-stylus3+ +btn-task+ +btn-thumb+ +btn-thumb2+ +btn-thumbl+
    +btn-thumbr+ +btn-tl+ +btn-tl2+ +btn-tool-airbrush+ +btn-tool-brush+
    +btn-tool-doubletap+ +btn-tool-finger+ +btn-tool-lens+ +btn-tool-mouse+
    +btn-tool-pen+ +btn-tool-pencil+ +btn-tool-quadtap+ +btn-tool-quinttap+
    +btn-tool-rubber+ +btn-tool-tripletap+ +btn-top+ +btn-top2+ +btn-touch+
    +btn-tr+ +btn-tr2+ +btn-trigger+ +btn-trigger-happy+ +btn-trigger-happy1+
    +btn-trigger-happy10+ +btn-trigger-happy11+ +btn-trigger-happy12+
    +btn-trigger-happy13+ +btn-trigger-happy14+ +btn-trigger-happy15+
    +btn-trigger-happy16+ +btn-trigger-happy17+ +btn-trigger-happy18+
    +btn-trigger-happy19+ +btn-trigger-happy2+ +btn-trigger-happy20+
    +btn-trigger-happy21+ +btn-trigger-happy22+ +btn-trigger-happy23+
    +btn-trigger-happy24+ +btn-trigger-happy25+ +btn-trigger-happy26+
    +btn-trigger-happy27+ +btn-trigger-happy28+ +btn-trigger-happy29+
    +btn-trigger-happy3+ +btn-trigger-happy30+ +btn-trigger-happy31+
    +btn-trigger-happy32+ +btn-trigger-happy33+ +btn-trigger-happy34+
    +btn-trigger-happy35+ +btn-trigger-happy36+ +btn-trigger-happy37+
    +btn-trigger-happy38+ +btn-trigger-happy39+ +btn-trigger-happy4+
    +btn-trigger-happy40+ +btn-trigger-happy5+ +btn-trigger-happy6+
    +btn-trigger-happy7+ +btn-trigger-happy8+ +btn-trigger-happy9+ +btn-west+
    +btn-wheel+ +btn-x+ +btn-y+ +btn-z+ +ev-abs+ +ev-cnt+ +ev-ff+ +ev-ff-status+
    +ev-key+ +ev-led+ +ev-max+ +ev-msc+ +ev-pwr+ +ev-rel+ +ev-rep+ +ev-snd+
    +ev-sw+ +ev-syn+ +input-prop-accelerometer+ +input-prop-buttonpad+
    +input-prop-cnt+ +input-prop-direct+ +input-prop-max+ +input-prop-pointer+
    +input-prop-pointing-stick+ +input-prop-semi-mt+ +input-prop-topbuttonpad+
    +key-0+ +key-1+ +key-102nd+ +key-10channelsdown+ +key-10channelsup+ +key-2+
    +key-3+ +key-3d-mode+ +key-4+ +key-5+ +key-6+ +key-7+ +key-8+ +key-9+ +key-a+
    +key-ab+ +key-addressbook+ +key-again+ +key-als-toggle+ +key-alterase+
    +key-angle+ +key-apostrophe+ +key-appselect+ +key-archive+ +key-aspect-ratio+
    +key-assistant+ +key-attendant-off+ +key-attendant-on+ +key-attendant-toggle+
    +key-audio+ +key-audio-desc+ +key-aux+ +key-b+ +key-back+ +key-backslash+
    +key-backspace+ +key-bassboost+ +key-battery+ +key-blue+ +key-bluetooth+
    +key-bookmarks+ +key-break+ +key-brightness-auto+ +key-brightness-cycle+
    +key-brightness-max+ +key-brightness-min+ +key-brightness-toggle+
    +key-brightness-zero+ +key-brightnessdown+ +key-brightnessup+ +key-brl-dot1+
    +key-brl-dot10+ +key-brl-dot2+ +key-brl-dot3+ +key-brl-dot4+ +key-brl-dot5+
    +key-brl-dot6+ +key-brl-dot7+ +key-brl-dot8+ +key-brl-dot9+ +key-buttonconfig+
    +key-c+ +key-calc+ +key-calendar+ +key-camera+ +key-camera-down+
    +key-camera-focus+ +key-camera-left+ +key-camera-right+ +key-camera-up+
    +key-camera-zoomin+ +key-camera-zoomout+ +key-cancel+ +key-capslock+ +key-cd+
    +key-channel+ +key-channeldown+ +key-channelup+ +key-chat+ +key-clear+
    +key-close+ +key-closecd+ +key-cnt+ +key-coffee+ +key-comma+ +key-compose+
    +key-computer+ +key-config+ +key-connect+ +key-context-menu+
    +key-controlpanel+ +key-copy+ +key-cut+ +key-cyclewindows+ +key-d+
    +key-dashboard+ +key-data+ +key-database+ +key-del-eol+ +key-del-eos+
    +key-del-line+ +key-delete+ +key-deletefile+ +key-digits+ +key-direction+
    +key-directory+ +key-display-off+ +key-displaytoggle+ +key-documents+
    +key-dollar+ +key-dot+ +key-down+ +key-dvd+ +key-e+ +key-edit+ +key-editor+
    +key-ejectcd+ +key-ejectclosecd+ +key-email+ +key-end+ +key-enter+ +key-epg+
    +key-equal+ +key-esc+ +key-euro+ +key-exit+ +key-f+ +key-f1+ +key-f10+
    +key-f11+ +key-f12+ +key-f13+ +key-f14+ +key-f15+ +key-f16+ +key-f17+
    +key-f18+ +key-f19+ +key-f2+ +key-f20+ +key-f21+ +key-f22+ +key-f23+ +key-f24+
    +key-f3+ +key-f4+ +key-f5+ +key-f6+ +key-f7+ +key-f8+ +key-f9+
    +key-fastforward+ +key-fastreverse+ +key-favorites+ +key-file+ +key-finance+
    +key-find+ +key-first+ +key-fn+ +key-fn-1+ +key-fn-2+ +key-fn-b+ +key-fn-d+
    +key-fn-e+ +key-fn-esc+ +key-fn-f+ +key-fn-f1+ +key-fn-f10+ +key-fn-f11+
    +key-fn-f12+ +key-fn-f2+ +key-fn-f3+ +key-fn-f4+ +key-fn-f5+ +key-fn-f6+
    +key-fn-f7+ +key-fn-f8+ +key-fn-f9+ +key-fn-s+ +key-forward+ +key-forwardmail+
    +key-frameback+ +key-frameforward+ +key-front+ +key-full-screen+ +key-g+
    +key-games+ +key-goto+ +key-graphicseditor+ +key-grave+ +key-green+ +key-h+
    +key-hangeul+ +key-hanguel+ +key-hanja+ +key-help+ +key-henkan+ +key-hiragana+
    +key-home+ +key-homepage+ +key-hp+ +key-i+ +key-images+ +key-info+
    +key-ins-line+ +key-insert+ +key-iso+ +key-j+ +key-journal+ +key-k+
    +key-katakana+ +key-katakanahiragana+ +key-kbd-layout-next+
    +key-kbd-lcd-menu1+ +key-kbd-lcd-menu2+ +key-kbd-lcd-menu3+
    +key-kbd-lcd-menu4+ +key-kbd-lcd-menu5+ +key-kbdillumdown+
    +key-kbdillumtoggle+ +key-kbdillumup+ +key-kbdinputassist-accept+
    +key-kbdinputassist-cancel+ +key-kbdinputassist-next+
    +key-kbdinputassist-nextgroup+ +key-kbdinputassist-prev+
    +key-kbdinputassist-prevgroup+ +key-keyboard+ +key-kp0+ +key-kp1+ +key-kp2+
    +key-kp3+ +key-kp4+ +key-kp5+ +key-kp6+ +key-kp7+ +key-kp8+ +key-kp9+
    +key-kpasterisk+ +key-kpcomma+ +key-kpdot+ +key-kpenter+ +key-kpequal+
    +key-kpjpcomma+ +key-kpleftparen+ +key-kpminus+ +key-kpplus+ +key-kpplusminus+
    +key-kprightparen+ +key-kpslash+ +key-l+ +key-language+ +key-last+ +key-left+
    +key-left-down+ +key-left-up+ +key-leftalt+ +key-leftbrace+ +key-leftctrl+
    +key-leftmeta+ +key-leftshift+ +key-lights-toggle+ +key-linefeed+ +key-list+
    +key-logoff+ +key-m+ +key-macro+ +key-macro-preset-cycle+ +key-macro-preset1+
    +key-macro-preset2+ +key-macro-preset3+ +key-macro-record-start+
    +key-macro-record-stop+ +key-macro1+ +key-macro10+ +key-macro11+ +key-macro12+
    +key-macro13+ +key-macro14+ +key-macro15+ +key-macro16+ +key-macro17+
    +key-macro18+ +key-macro19+ +key-macro2+ +key-macro20+ +key-macro21+
    +key-macro22+ +key-macro23+ +key-macro24+ +key-macro25+ +key-macro26+
    +key-macro27+ +key-macro28+ +key-macro29+ +key-macro3+ +key-macro30+
    +key-macro4+ +key-macro5+ +key-macro6+ +key-macro7+ +key-macro8+ +key-macro9+
    +key-mail+ +key-max+ +key-media+ +key-media-repeat+ +key-media-top-menu+
    +key-memo+ +key-menu+ +key-messenger+ +key-mhp+ +key-micmute+
    +key-min-interesting+ +key-minus+ +key-mode+ +key-move+ +key-mp3+ +key-msdos+
    +key-muhenkan+ +key-mute+ +key-n+ +key-new+ +key-news+ +key-next+
    +key-next-favorite+ +key-nextsong+ +key-numeric-0+ +key-numeric-1+
    +key-numeric-11+ +key-numeric-12+ +key-numeric-2+ +key-numeric-3+
    +key-numeric-4+ +key-numeric-5+ +key-numeric-6+ +key-numeric-7+
    +key-numeric-8+ +key-numeric-9+ +key-numeric-a+ +key-numeric-b+
    +key-numeric-c+ +key-numeric-d+ +key-numeric-pound+ +key-numeric-star+
    +key-numlock+ +key-o+ +key-ok+ +key-onscreen-keyboard+ +key-open+ +key-option+
    +key-p+ +key-pagedown+ +key-pageup+ +key-paste+ +key-pause+ +key-pause-record+
    +key-pausecd+ +key-pc+ +key-phone+ +key-play+ +key-playcd+ +key-player+
    +key-playpause+ +key-power+ +key-power2+ +key-presentation+ +key-previous+
    +key-previoussong+ +key-print+ +key-privacy-screen-toggle+ +key-prog1+
    +key-prog2+ +key-prog3+ +key-prog4+ +key-program+ +key-props+ +key-pvr+
    +key-q+ +key-question+ +key-r+ +key-radio+ +key-record+ +key-red+ +key-redo+
    +key-refresh+ +key-reply+ +key-reserved+ +key-restart+ +key-rewind+
    +key-rfkill+ +key-right+ +key-right-down+ +key-right-up+ +key-rightalt+
    +key-rightbrace+ +key-rightctrl+ +key-rightmeta+ +key-rightshift+ +key-ro+
    +key-root-menu+ +key-rotate-display+ +key-rotate-lock-toggle+ +key-s+
    +key-sat+ +key-sat2+ +key-save+ +key-scale+ +key-screen+ +key-screenlock+
    +key-screensaver+ +key-scrolldown+ +key-scrolllock+ +key-scrollup+
    +key-search+ +key-select+ +key-selective-screenshot+ +key-semicolon+
    +key-send+ +key-sendfile+ +key-setup+ +key-shop+ +key-shuffle+ +key-slash+
    +key-sleep+ +key-slow+ +key-slowreverse+ +key-sound+ +key-space+
    +key-spellcheck+ +key-sport+ +key-spreadsheet+ +key-stop+ +key-stop-record+
    +key-stopcd+ +key-subtitle+ +key-suspend+ +key-switchvideomode+ +key-sysrq+
    +key-t+ +key-tab+ +key-tape+ +key-taskmanager+ +key-teen+ +key-text+
    +key-time+ +key-title+ +key-touchpad-off+ +key-touchpad-on+
    +key-touchpad-toggle+ +key-tuner+ +key-tv+ +key-tv2+ +key-twen+ +key-u+
    +key-undo+ +key-unknown+ +key-unmute+ +key-up+ +key-uwb+ +key-v+ +key-vcr+
    +key-vcr2+ +key-vendor+ +key-video+ +key-video-next+ +key-video-prev+
    +key-videophone+ +key-vod+ +key-voicecommand+ +key-voicemail+ +key-volumedown+
    +key-volumeup+ +key-w+ +key-wakeup+ +key-wimax+ +key-wlan+ +key-wordprocessor+
    +key-wps-button+ +key-wwan+ +key-www+ +key-x+ +key-xfer+ +key-y+ +key-yellow+
    +key-yen+ +key-z+ +key-zenkakuhankaku+ +key-zoom+ +key-zoomin+ +key-zoomout+
    +key-zoomreset+ +led-capsl+ +led-charging+ +led-cnt+ +led-compose+ +led-kana+
    +led-mail+ +led-max+ +led-misc+ +led-mute+ +led-numl+ +led-scrolll+
    +led-sleep+ +led-suspend+ +msc-cnt+ +msc-gesture+ +msc-max+ +msc-pulseled+
    +msc-raw+ +msc-scan+ +msc-serial+ +msc-timestamp+ +rel-cnt+ +rel-dial+
    +rel-hwheel+ +rel-hwheel-hi-res+ +rel-max+ +rel-misc+ +rel-reserved+ +rel-rx+
    +rel-ry+ +rel-rz+ +rel-wheel+ +rel-wheel-hi-res+ +rel-x+ +rel-y+ +rel-z+
    +rep-cnt+ +rep-delay+ +rep-max+ +rep-period+ +snd-bell+ +snd-click+ +snd-cnt+
    +snd-max+ +snd-tone+ +sw-camera-lens-cover+ +sw-cnt+ +sw-dock+
    +sw-front-proximity+ +sw-headphone-insert+ +sw-jack-physical-insert+
    +sw-keypad-slide+ +sw-lid+ +sw-linein-insert+ +sw-lineout-insert+ +sw-max+
    +sw-microphone-insert+ +sw-mute-device+ +sw-pen-inserted+ +sw-radio+
    +sw-rfkill-all+ +sw-rotate-lock+ +sw-tablet-mode+ +sw-videoout-insert+
    +syn-cnt+ +syn-config+ +syn-dropped+ +syn-max+ +syn-mt-report+ +syn-report+))

(in-package #:xyz.shunter.input-event-codes)




;; Device properties and quirks

(defconstant +input-prop-pointer+ #x00
  "needs a pointer")
(defconstant +input-prop-direct+ #x01
  "direct input devices")
(defconstant +input-prop-buttonpad+ #x02
  "has button(s) under pad")
(defconstant +input-prop-semi-mt+ #x03
  "touch rectangle only")
(defconstant +input-prop-topbuttonpad+ #x04
  "softbuttons at top of pad")
(defconstant +input-prop-pointing-stick+ #x05
  "is a pointing stick")
(defconstant +input-prop-accelerometer+ #x06
  "has accelerometer")

(defconstant +input-prop-max+ #x1f)
(defconstant +input-prop-cnt+ (1+ +input-prop-max+))

;; Event types

(defconstant +ev-syn+ #x00)
(defconstant +ev-key+ #x01)
(defconstant +ev-rel+ #x02)
(defconstant +ev-abs+ #x03)
(defconstant +ev-msc+ #x04)
(defconstant +ev-sw+ #x05)
(defconstant +ev-led+ #x11)
(defconstant +ev-snd+ #x12)
(defconstant +ev-rep+ #x14)
(defconstant +ev-ff+ #x15)
(defconstant +ev-pwr+ #x16)
(defconstant +ev-ff-status+ #x17)
(defconstant +ev-max+ #x1f)
(defconstant +ev-cnt+ (1+ +ev-max+))

;; Synchronization events.

(defconstant +syn-report+ 0)
(defconstant +syn-config+ 1)
(defconstant +syn-mt-report+ 2)
(defconstant +syn-dropped+ 3)
(defconstant +syn-max+ #xf)
(defconstant +syn-cnt+ (1+ +syn-max+))

;; Keys and buttons

;; Abbreviations in the comments:
;; AC - Application Control
;; AL - Application Launch Button
;; SC - System Control

(defconstant +key-reserved+ 0)
(defconstant +key-esc+ 1)
(defconstant +key-1+ 2)
(defconstant +key-2+ 3)
(defconstant +key-3+ 4)
(defconstant +key-4+ 5)
(defconstant +key-5+ 6)
(defconstant +key-6+ 7)
(defconstant +key-7+ 8)
(defconstant +key-8+ 9)
(defconstant +key-9+ 10)
(defconstant +key-0+ 11)
(defconstant +key-minus+ 12)
(defconstant +key-equal+ 13)
(defconstant +key-backspace+ 14)
(defconstant +key-tab+ 15)
(defconstant +key-q+ 16)
(defconstant +key-w+ 17)
(defconstant +key-e+ 18)
(defconstant +key-r+ 19)
(defconstant +key-t+ 20)
(defconstant +key-y+ 21)
(defconstant +key-u+ 22)
(defconstant +key-i+ 23)
(defconstant +key-o+ 24)
(defconstant +key-p+ 25)
(defconstant +key-leftbrace+ 26)
(defconstant +key-rightbrace+ 27)
(defconstant +key-enter+ 28)
(defconstant +key-leftctrl+ 29)
(defconstant +key-a+ 30)
(defconstant +key-s+ 31)
(defconstant +key-d+ 32)
(defconstant +key-f+ 33)
(defconstant +key-g+ 34)
(defconstant +key-h+ 35)
(defconstant +key-j+ 36)
(defconstant +key-k+ 37)
(defconstant +key-l+ 38)
(defconstant +key-semicolon+ 39)
(defconstant +key-apostrophe+ 40)
(defconstant +key-grave+ 41)
(defconstant +key-leftshift+ 42)
(defconstant +key-backslash+ 43)
(defconstant +key-z+ 44)
(defconstant +key-x+ 45)
(defconstant +key-c+ 46)
(defconstant +key-v+ 47)
(defconstant +key-b+ 48)
(defconstant +key-n+ 49)
(defconstant +key-m+ 50)
(defconstant +key-comma+ 51)
(defconstant +key-dot+ 52)
(defconstant +key-slash+ 53)
(defconstant +key-rightshift+ 54)
(defconstant +key-kpasterisk+ 55)
(defconstant +key-leftalt+ 56)
(defconstant +key-space+ 57)
(defconstant +key-capslock+ 58)
(defconstant +key-f1+ 59)
(defconstant +key-f2+ 60)
(defconstant +key-f3+ 61)
(defconstant +key-f4+ 62)
(defconstant +key-f5+ 63)
(defconstant +key-f6+ 64)
(defconstant +key-f7+ 65)
(defconstant +key-f8+ 66)
(defconstant +key-f9+ 67)
(defconstant +key-f10+ 68)
(defconstant +key-numlock+ 69)
(defconstant +key-scrolllock+ 70)
(defconstant +key-kp7+ 71)
(defconstant +key-kp8+ 72)
(defconstant +key-kp9+ 73)
(defconstant +key-kpminus+ 74)
(defconstant +key-kp4+ 75)
(defconstant +key-kp5+ 76)
(defconstant +key-kp6+ 77)
(defconstant +key-kpplus+ 78)
(defconstant +key-kp1+ 79)
(defconstant +key-kp2+ 80)
(defconstant +key-kp3+ 81)
(defconstant +key-kp0+ 82)
(defconstant +key-kpdot+ 83)

(defconstant +key-zenkakuhankaku+ 85)
(defconstant +key-102nd+ 86)
(defconstant +key-f11+ 87)
(defconstant +key-f12+ 88)
(defconstant +key-ro+ 89)
(defconstant +key-katakana+ 90)
(defconstant +key-hiragana+ 91)
(defconstant +key-henkan+ 92)
(defconstant +key-katakanahiragana+ 93)
(defconstant +key-muhenkan+ 94)
(defconstant +key-kpjpcomma+ 95)
(defconstant +key-kpenter+ 96)
(defconstant +key-rightctrl+ 97)
(defconstant +key-kpslash+ 98)
(defconstant +key-sysrq+ 99)
(defconstant +key-rightalt+ 100)
(defconstant +key-linefeed+ 101)
(defconstant +key-home+ 102)
(defconstant +key-up+ 103)
(defconstant +key-pageup+ 104)
(defconstant +key-left+ 105)
(defconstant +key-right+ 106)
(defconstant +key-end+ 107)
(defconstant +key-down+ 108)
(defconstant +key-pagedown+ 109)
(defconstant +key-insert+ 110)
(defconstant +key-delete+ 111)
(defconstant +key-macro+ 112)
(defconstant +key-mute+ 113)
(defconstant +key-volumedown+ 114)
(defconstant +key-volumeup+ 115)
(defconstant +key-power+ 116
  "SC System Power Down")
(defconstant +key-kpequal+ 117)
(defconstant +key-kpplusminus+ 118)
(defconstant +key-pause+ 119)
(defconstant +key-scale+ 120
  "AL Compiz Scale (Expose)")
(defconstant +key-kpcomma+ 121)
(defconstant +key-hangeul+ 122)
(defconstant +key-hanguel+ +key-hangeul+)
(defconstant +key-hanja+ 123)
(defconstant +key-yen+ 124)
(defconstant +key-leftmeta+ 125)
(defconstant +key-rightmeta+ 126)
(defconstant +key-compose+ 127)
(defconstant +key-stop+ 128
  "AC Stop")
(defconstant +key-again+ 129)
(defconstant +key-props+ 130
  "AC Properties")
(defconstant +key-undo+ 131
  "AC Undo")
(defconstant +key-front+ 132)
(defconstant +key-copy+ 133
  "AC Copy")
(defconstant +key-open+ 134
  "AC Open")
(defconstant +key-paste+ 135
  "AC Paste")
(defconstant +key-find+ 136
  "AC Search")
(defconstant +key-cut+ 137
  "AC Cut")
(defconstant +key-help+ 138
  "AL Integrated Help Center")
(defconstant +key-menu+ 139
  "Menu (show menu)")
(defconstant +key-calc+ 140
  "AL Calculator")
(defconstant +key-setup+ 141)
(defconstant +key-sleep+ 142
  "SC System Sleep")
(defconstant +key-wakeup+ 143
  "System Wake Up")
(defconstant +key-file+ 144
  "AL Local Machine Browser")
(defconstant +key-sendfile+ 145)
(defconstant +key-deletefile+ 146)
(defconstant +key-xfer+ 147)
(defconstant +key-prog1+ 148)
(defconstant +key-prog2+ 149)
(defconstant +key-www+ 150
  "AL Internet Browser")
(defconstant +key-msdos+ 151)
(defconstant +key-coffee+ 152
  "AL Terminal Lock/Screensaver")
(defconstant +key-screenlock+ +key-coffee+)
(defconstant +key-rotate-display+ 153
  "Display orientation for e.g. tablets")
(defconstant +key-direction+ +key-rotate-display+)
(defconstant +key-cyclewindows+ 154)
(defconstant +key-mail+ 155)
(defconstant +key-bookmarks+ 156
  "AC Bookmarks")
(defconstant +key-computer+ 157)
(defconstant +key-back+ 158
  "AC Back")
(defconstant +key-forward+ 159
  "AC Forward")
(defconstant +key-closecd+ 160)
(defconstant +key-ejectcd+ 161)
(defconstant +key-ejectclosecd+ 162)
(defconstant +key-nextsong+ 163)
(defconstant +key-playpause+ 164)
(defconstant +key-previoussong+ 165)
(defconstant +key-stopcd+ 166)
(defconstant +key-record+ 167)
(defconstant +key-rewind+ 168)
(defconstant +key-phone+ 169
  "Media Select Telephone")
(defconstant +key-iso+ 170)
(defconstant +key-config+ 171
  "AL Consumer Control Configuration")
(defconstant +key-homepage+ 172
  "AC Home")
(defconstant +key-refresh+ 173
  "AC Refresh")
(defconstant +key-exit+ 174
  "AC Exit")
(defconstant +key-move+ 175)
(defconstant +key-edit+ 176)
(defconstant +key-scrollup+ 177)
(defconstant +key-scrolldown+ 178)
(defconstant +key-kpleftparen+ 179)
(defconstant +key-kprightparen+ 180)
(defconstant +key-new+ 181
  "AC New")
(defconstant +key-redo+ 182
  "AC Redo/Repeat")

(defconstant +key-f13+ 183)
(defconstant +key-f14+ 184)
(defconstant +key-f15+ 185)
(defconstant +key-f16+ 186)
(defconstant +key-f17+ 187)
(defconstant +key-f18+ 188)
(defconstant +key-f19+ 189)
(defconstant +key-f20+ 190)
(defconstant +key-f21+ 191)
(defconstant +key-f22+ 192)
(defconstant +key-f23+ 193)
(defconstant +key-f24+ 194)

(defconstant +key-playcd+ 200)
(defconstant +key-pausecd+ 201)
(defconstant +key-prog3+ 202)
(defconstant +key-prog4+ 203)
(defconstant +key-dashboard+ 204
  "AL Dashboard")
(defconstant +key-suspend+ 205)
(defconstant +key-close+ 206
  "AC Close")
(defconstant +key-play+ 207)
(defconstant +key-fastforward+ 208)
(defconstant +key-bassboost+ 209)
(defconstant +key-print+ 210
  "AC Print")
(defconstant +key-hp+ 211)
(defconstant +key-camera+ 212)
(defconstant +key-sound+ 213)
(defconstant +key-question+ 214)
(defconstant +key-email+ 215)
(defconstant +key-chat+ 216)
(defconstant +key-search+ 217)
(defconstant +key-connect+ 218)
(defconstant +key-finance+ 219
  "AL Checkbook/Finance")
(defconstant +key-sport+ 220)
(defconstant +key-shop+ 221)
(defconstant +key-alterase+ 222)
(defconstant +key-cancel+ 223
  "AC Cancel")
(defconstant +key-brightnessdown+ 224)
(defconstant +key-brightnessup+ 225)
(defconstant +key-media+ 226)

(defconstant +key-switchvideomode+ 227 "Cycle between available video outputs (Monitor/LCD/TV-out/etc)")
(defconstant +key-kbdillumtoggle+ 228)
(defconstant +key-kbdillumdown+ 229)
(defconstant +key-kbdillumup+ 230)

(defconstant +key-send+ 231
  "AC Send")
(defconstant +key-reply+ 232
  "AC Reply")
(defconstant +key-forwardmail+ 233
  "AC Forward Msg")
(defconstant +key-save+ 234
  "AC Save")
(defconstant +key-documents+ 235)

(defconstant +key-battery+ 236)

(defconstant +key-bluetooth+ 237)
(defconstant +key-wlan+ 238)
(defconstant +key-uwb+ 239)

(defconstant +key-unknown+ 240)

(defconstant +key-video-next+ 241
  "drive next video source")
(defconstant +key-video-prev+ 242
  "drive previous video source")
(defconstant +key-brightness-cycle+ 243
  "brightness up, after max is min")
(defconstant +key-brightness-auto+ 244
  "Set Auto Brightness: manual brightness control is off, rely on ambient ")
(defconstant +key-brightness-zero+ +key-brightness-auto+)
(defconstant +key-display-off+ 245
  "display device to off state")

(defconstant +key-wwan+ 246
  "Wireless WAN (LTE, UMTS, GSM, etc.)")
(defconstant +key-wimax+ +key-wwan+)
(defconstant +key-rfkill+ 247
  "Key that controls all radios")

(defconstant +key-micmute+ 248
  "Mute / unmute the microphone")

;; Code 255 is reserved for special needs of AT keyboard driver

(defconstant +btn-misc+ #x100)
(defconstant +btn-0+ #x100)
(defconstant +btn-1+ #x101)
(defconstant +btn-2+ #x102)
(defconstant +btn-3+ #x103)
(defconstant +btn-4+ #x104)
(defconstant +btn-5+ #x105)
(defconstant +btn-6+ #x106)
(defconstant +btn-7+ #x107)
(defconstant +btn-8+ #x108)
(defconstant +btn-9+ #x109)

(defconstant +btn-mouse+ #x110)
(defconstant +btn-left+ #x110)
(defconstant +btn-right+ #x111)
(defconstant +btn-middle+ #x112)
(defconstant +btn-side+ #x113)
(defconstant +btn-extra+ #x114)
(defconstant +btn-forward+ #x115)
(defconstant +btn-back+ #x116)
(defconstant +btn-task+ #x117)

(defconstant +btn-joystick+ #x120)
(defconstant +btn-trigger+ #x120)
(defconstant +btn-thumb+ #x121)
(defconstant +btn-thumb2+ #x122)
(defconstant +btn-top+ #x123)
(defconstant +btn-top2+ #x124)
(defconstant +btn-pinkie+ #x125)
(defconstant +btn-base+ #x126)
(defconstant +btn-base2+ #x127)
(defconstant +btn-base3+ #x128)
(defconstant +btn-base4+ #x129)
(defconstant +btn-base5+ #x12a)
(defconstant +btn-base6+ #x12b)
(defconstant +btn-dead+ #x12f)

(defconstant +btn-gamepad+ #x130)
(defconstant +btn-south+ #x130)
(defconstant +btn-a+ +btn-south+)
(defconstant +btn-east+ #x131)
(defconstant +btn-b+ +btn-east+)
(defconstant +btn-c+ #x132)
(defconstant +btn-north+ #x133)
(defconstant +btn-x+ +btn-north+)
(defconstant +btn-west+ #x134)
(defconstant +btn-y+ +btn-west+)
(defconstant +btn-z+ #x135)
(defconstant +btn-tl+ #x136)
(defconstant +btn-tr+ #x137)
(defconstant +btn-tl2+ #x138)
(defconstant +btn-tr2+ #x139)
(defconstant +btn-select+ #x13a)
(defconstant +btn-start+ #x13b)
(defconstant +btn-mode+ #x13c)
(defconstant +btn-thumbl+ #x13d)
(defconstant +btn-thumbr+ #x13e)

(defconstant +btn-digi+ #x140)
(defconstant +btn-tool-pen+ #x140)
(defconstant +btn-tool-rubber+ #x141)
(defconstant +btn-tool-brush+ #x142)
(defconstant +btn-tool-pencil+ #x143)
(defconstant +btn-tool-airbrush+ #x144)
(defconstant +btn-tool-finger+ #x145)
(defconstant +btn-tool-mouse+ #x146)
(defconstant +btn-tool-lens+ #x147)
(defconstant +btn-tool-quinttap+ #x148
  "Five fingers on trackpad")
(defconstant +btn-stylus3+ #x149)
(defconstant +btn-touch+ #x14a)
(defconstant +btn-stylus+ #x14b)
(defconstant +btn-stylus2+ #x14c)
(defconstant +btn-tool-doubletap+ #x14d)
(defconstant +btn-tool-tripletap+ #x14e)
(defconstant +btn-tool-quadtap+ #x14f
  "Four fingers on trackpad")

(defconstant +btn-wheel+ #x150)
(defconstant +btn-gear-down+ #x150)
(defconstant +btn-gear-up+ #x151)

(defconstant +key-ok+ #x160)
(defconstant +key-select+ #x161)
(defconstant +key-goto+ #x162)
(defconstant +key-clear+ #x163)
(defconstant +key-power2+ #x164)
(defconstant +key-option+ #x165)
(defconstant +key-info+ #x166
  "AL OEM Features/Tips/Tutorial")
(defconstant +key-time+ #x167)
(defconstant +key-vendor+ #x168)
(defconstant +key-archive+ #x169)
(defconstant +key-program+ #x16a
  "Media Select Program Guide")
(defconstant +key-channel+ #x16b)
(defconstant +key-favorites+ #x16c)
(defconstant +key-epg+ #x16d)
(defconstant +key-pvr+ #x16e
  "Media Select Home")
(defconstant +key-mhp+ #x16f)
(defconstant +key-language+ #x170)
(defconstant +key-title+ #x171)
(defconstant +key-subtitle+ #x172)
(defconstant +key-angle+ #x173)
(defconstant +key-full-screen+ #x174
  "AC View Toggle")
(defconstant +key-zoom+ +key-full-screen+)
(defconstant +key-mode+ #x175)
(defconstant +key-keyboard+ #x176)
(defconstant +key-aspect-ratio+ #x177
  "HUTRR37: Aspect")
(defconstant +key-screen+ +key-aspect-ratio+)
(defconstant +key-pc+ #x178
  "Media Select Computer")
(defconstant +key-tv+ #x179
  "Media Select TV")
(defconstant +key-tv2+ #x17a
  "Media Select Cable")
(defconstant +key-vcr+ #x17b
  "Media Select VCR")
(defconstant +key-vcr2+ #x17c
  "VCR Plus")
(defconstant +key-sat+ #x17d
  "Media Select Satellite")
(defconstant +key-sat2+ #x17e)
(defconstant +key-cd+ #x17f
  "Media Select CD")
(defconstant +key-tape+ #x180
  "Media Select Tape")
(defconstant +key-radio+ #x181)
(defconstant +key-tuner+ #x182
  "Media Select Tuner")
(defconstant +key-player+ #x183)
(defconstant +key-text+ #x184)
(defconstant +key-dvd+ #x185
  "Media Select DVD")
(defconstant +key-aux+ #x186)
(defconstant +key-mp3+ #x187)
(defconstant +key-audio+ #x188
  "AL Audio Browser")
(defconstant +key-video+ #x189
  "AL Movie Browser")
(defconstant +key-directory+ #x18a)
(defconstant +key-list+ #x18b)
(defconstant +key-memo+ #x18c
  "Media Select Messages")
(defconstant +key-calendar+ #x18d)
(defconstant +key-red+ #x18e)
(defconstant +key-green+ #x18f)
(defconstant +key-yellow+ #x190)
(defconstant +key-blue+ #x191)
(defconstant +key-channelup+ #x192
  "Channel Increment")
(defconstant +key-channeldown+ #x193
  "Channel Decrement")
(defconstant +key-first+ #x194)
(defconstant +key-last+ #x195
  "Recall Last")
(defconstant +key-ab+ #x196)
(defconstant +key-next+ #x197)
(defconstant +key-restart+ #x198)
(defconstant +key-slow+ #x199)
(defconstant +key-shuffle+ #x19a)
(defconstant +key-break+ #x19b)
(defconstant +key-previous+ #x19c)
(defconstant +key-digits+ #x19d)
(defconstant +key-teen+ #x19e)
(defconstant +key-twen+ #x19f)
(defconstant +key-videophone+ #x1a0
  "Media Select Video Phone")
(defconstant +key-games+ #x1a1
  "Media Select Games")
(defconstant +key-zoomin+ #x1a2
  "AC Zoom In")
(defconstant +key-zoomout+ #x1a3
  "AC Zoom Out")
(defconstant +key-zoomreset+ #x1a4
  "AC Zoom")
(defconstant +key-wordprocessor+ #x1a5
  "AL Word Processor")
(defconstant +key-editor+ #x1a6
  "AL Text Editor")
(defconstant +key-spreadsheet+ #x1a7
  "AL Spreadsheet")
(defconstant +key-graphicseditor+ #x1a8
  "AL Graphics Editor")
(defconstant +key-presentation+ #x1a9
  "AL Presentation App")
(defconstant +key-database+ #x1aa
  "AL Database App")
(defconstant +key-news+ #x1ab
  "AL Newsreader")
(defconstant +key-voicemail+ #x1ac
  "AL Voicemail")
(defconstant +key-addressbook+ #x1ad
  "AL Contacts/Address Book")
(defconstant +key-messenger+ #x1ae
  "AL Instant Messaging")
(defconstant +key-displaytoggle+ #x1af
  "Turn display (LCD) on and off")
(defconstant +key-brightness-toggle+ +key-displaytoggle+)
(defconstant +key-spellcheck+ #x1b0
  "AL Spell Check")
(defconstant +key-logoff+ #x1b1
  "AL Logoff")

(defconstant +key-dollar+ #x1b2)
(defconstant +key-euro+ #x1b3)

(defconstant +key-frameback+ #x1b4
  "Consumer - transport controls")
(defconstant +key-frameforward+ #x1b5)
(defconstant +key-context-menu+ #x1b6
  "GenDesc - system context menu")
(defconstant +key-media-repeat+ #x1b7
  "Consumer - transport control")
(defconstant +key-10channelsup+ #x1b8
  "10 channels up (10+)")
(defconstant +key-10channelsdown+ #x1b9
  "10 channels down (10-)")
(defconstant +key-images+ #x1ba
  "AL Image Browser")

(defconstant +key-del-eol+ #x1c0)
(defconstant +key-del-eos+ #x1c1)
(defconstant +key-ins-line+ #x1c2)
(defconstant +key-del-line+ #x1c3)

(defconstant +key-fn+ #x1d0)
(defconstant +key-fn-esc+ #x1d1)
(defconstant +key-fn-f1+ #x1d2)
(defconstant +key-fn-f2+ #x1d3)
(defconstant +key-fn-f3+ #x1d4)
(defconstant +key-fn-f4+ #x1d5)
(defconstant +key-fn-f5+ #x1d6)
(defconstant +key-fn-f6+ #x1d7)
(defconstant +key-fn-f7+ #x1d8)
(defconstant +key-fn-f8+ #x1d9)
(defconstant +key-fn-f9+ #x1da)
(defconstant +key-fn-f10+ #x1db)
(defconstant +key-fn-f11+ #x1dc)
(defconstant +key-fn-f12+ #x1dd)
(defconstant +key-fn-1+ #x1de)
(defconstant +key-fn-2+ #x1df)
(defconstant +key-fn-d+ #x1e0)
(defconstant +key-fn-e+ #x1e1)
(defconstant +key-fn-f+ #x1e2)
(defconstant +key-fn-s+ #x1e3)
(defconstant +key-fn-b+ #x1e4)

(defconstant +key-brl-dot1+ #x1f1)
(defconstant +key-brl-dot2+ #x1f2)
(defconstant +key-brl-dot3+ #x1f3)
(defconstant +key-brl-dot4+ #x1f4)
(defconstant +key-brl-dot5+ #x1f5)
(defconstant +key-brl-dot6+ #x1f6)
(defconstant +key-brl-dot7+ #x1f7)
(defconstant +key-brl-dot8+ #x1f8)
(defconstant +key-brl-dot9+ #x1f9)
(defconstant +key-brl-dot10+ #x1fa)

(defconstant +key-numeric-0+ #x200
  "used by phones, remote controls,")
(defconstant +key-numeric-1+ #x201
  "and other keypads")
(defconstant +key-numeric-2+ #x202)
(defconstant +key-numeric-3+ #x203)
(defconstant +key-numeric-4+ #x204)
(defconstant +key-numeric-5+ #x205)
(defconstant +key-numeric-6+ #x206)
(defconstant +key-numeric-7+ #x207)
(defconstant +key-numeric-8+ #x208)
(defconstant +key-numeric-9+ #x209)
(defconstant +key-numeric-star+ #x20a)
(defconstant +key-numeric-pound+ #x20b)
(defconstant +key-numeric-a+ #x20c
  "Phone key A - HUT Telephony #xb9")
(defconstant +key-numeric-b+ #x20d)
(defconstant +key-numeric-c+ #x20e)
(defconstant +key-numeric-d+ #x20f)

(defconstant +key-camera-focus+ #x210)
(defconstant +key-wps-button+ #x211
  "WiFi Protected Setup key")

(defconstant +key-touchpad-toggle+ #x212
  "Request switch touchpad on or off")
(defconstant +key-touchpad-on+ #x213)
(defconstant +key-touchpad-off+ #x214)

(defconstant +key-camera-zoomin+ #x215)
(defconstant +key-camera-zoomout+ #x216)
(defconstant +key-camera-up+ #x217)
(defconstant +key-camera-down+ #x218)
(defconstant +key-camera-left+ #x219)
(defconstant +key-camera-right+ #x21a)

(defconstant +key-attendant-on+ #x21b)
(defconstant +key-attendant-off+ #x21c)
(defconstant +key-attendant-toggle+ #x21d
  "Attendant call on or off")
(defconstant +key-lights-toggle+ #x21e
  "Reading light on or off")

(defconstant +btn-dpad-up+ #x220)
(defconstant +btn-dpad-down+ #x221)
(defconstant +btn-dpad-left+ #x222)
(defconstant +btn-dpad-right+ #x223)

(defconstant +key-als-toggle+ #x230
  "Ambient light sensor")
(defconstant +key-rotate-lock-toggle+ #x231
  "Display rotation lock")

(defconstant +key-buttonconfig+ #x240
  "AL Button Configuration")
(defconstant +key-taskmanager+ #x241
  "AL Task/Project Manager")
(defconstant +key-journal+ #x242
  "AL Log/Journal/Timecard")
(defconstant +key-controlpanel+ #x243
  "AL Control Panel")
(defconstant +key-appselect+ #x244
  "AL Select Task/Application")
(defconstant +key-screensaver+ #x245
  "AL Screen Saver")
(defconstant +key-voicecommand+ #x246
  "Listening Voice Command")
(defconstant +key-assistant+ #x247
  "AL Context-aware desktop assistant")
(defconstant +key-kbd-layout-next+ #x248
  "AC Next Keyboard Layout Select")

(defconstant +key-brightness-min+ #x250
  "Set Brightness to Minimum")
(defconstant +key-brightness-max+ #x251
  "Set Brightness to Maximum")

(defconstant +key-kbdinputassist-prev+ #x260)
(defconstant +key-kbdinputassist-next+ #x261)
(defconstant +key-kbdinputassist-prevgroup+ #x262)
(defconstant +key-kbdinputassist-nextgroup+ #x263)
(defconstant +key-kbdinputassist-accept+ #x264)
(defconstant +key-kbdinputassist-cancel+ #x265)

;; Diagonal movement keys
(defconstant +key-right-up+ #x266)
(defconstant +key-right-down+ #x267)
(defconstant +key-left-up+ #x268)
(defconstant +key-left-down+ #x269)

(defconstant +key-root-menu+ #x26a
  "Show Device's Root Menu")
(defconstant +key-media-top-menu+ #x26b
  "Show Top Menu of the Media (e.g. DVD)")
(defconstant +key-numeric-11+ #x26c)
(defconstant +key-numeric-12+ #x26d)
;; Toggle Audio Description: refers to an audio service that helps blind and
;; visually impaired consumers understand the action in a program. Note: in
;; some countries this is referred to as "Video Description".
(defconstant +key-audio-desc+ #x26e)
(defconstant +key-3d-mode+ #x26f)
(defconstant +key-next-favorite+ #x270)
(defconstant +key-stop-record+ #x271)
(defconstant +key-pause-record+ #x272)
(defconstant +key-vod+ #x273
  "Video on Demand")
(defconstant +key-unmute+ #x274)
(defconstant +key-fastreverse+ #x275)
(defconstant +key-slowreverse+ #x276)
;; Control a data application associated with the currently viewed channel,
;; e.g. teletext or data broadcast application (MHEG, MHP, HbbTV, etc.)
(defconstant +key-data+ #x277)
(defconstant +key-onscreen-keyboard+ #x278)
(defconstant +key-privacy-screen-toggle+ #x279
  "Electronic privacy screen control")

;; Select an area of screen to be copied
(defconstant +key-selective-screenshot+ #x27a)

;; Some keyboards have keys which do not have a defined meaning, these keys
;; are intended to be programmed / bound to macros by the user. For most
;; keyboards with these macro-keys the key-sequence to inject, or action to
;; take, is all handled by software on the host side. So from the kernel's
;; point of view these are just normal keys.
;;
;; The +key-macro#+ codes below are intended for such keys, which may be labeled
;; e.g. G1-G18, or S1 - S30. The +key-macro#+ codes MUST NOT be used for keys
;; where the marking on the key does indicate a defined meaning / purpose.
;;
;; The +key-macro#+ codes MUST also NOT be used as fallback for when no existing
;; +key-foo+ define matches the marking / purpose. In this case a new +key-foo+
;; defconstant MUST be added.
(defconstant +key-macro1+ #x290)
(defconstant +key-macro2+ #x291)
(defconstant +key-macro3+ #x292)
(defconstant +key-macro4+ #x293)
(defconstant +key-macro5+ #x294)
(defconstant +key-macro6+ #x295)
(defconstant +key-macro7+ #x296)
(defconstant +key-macro8+ #x297)
(defconstant +key-macro9+ #x298)
(defconstant +key-macro10+ #x299)
(defconstant +key-macro11+ #x29a)
(defconstant +key-macro12+ #x29b)
(defconstant +key-macro13+ #x29c)
(defconstant +key-macro14+ #x29d)
(defconstant +key-macro15+ #x29e)
(defconstant +key-macro16+ #x29f)
(defconstant +key-macro17+ #x2a0)
(defconstant +key-macro18+ #x2a1)
(defconstant +key-macro19+ #x2a2)
(defconstant +key-macro20+ #x2a3)
(defconstant +key-macro21+ #x2a4)
(defconstant +key-macro22+ #x2a5)
(defconstant +key-macro23+ #x2a6)
(defconstant +key-macro24+ #x2a7)
(defconstant +key-macro25+ #x2a8)
(defconstant +key-macro26+ #x2a9)
(defconstant +key-macro27+ #x2aa)
(defconstant +key-macro28+ #x2ab)
(defconstant +key-macro29+ #x2ac)
(defconstant +key-macro30+ #x2ad)

;; Some keyboards with the macro-keys described above have some extra keys
;; for controlling the host-side software responsible for the macro handling:
;; -A macro recording start/stop key. Note that not all keyboards which emit
;;  KEY-MACRO-RECORD-START will also emit KEY-MACRO-RECORD-STOP if
;;  KEY-MACRO-RECORD-STOP is not advertised, then KEY-MACRO-RECORD-START
;;  should be interpreted as a recording start/stop toggle;
;; -Keys for switching between different macro (pre)sets, either a key for
;;  cycling through the configured presets or keys to directly select a preset.
(defconstant +key-macro-record-start+ #x2b0)
(defconstant +key-macro-record-stop+ #x2b1)
(defconstant +key-macro-preset-cycle+ #x2b2)
(defconstant +key-macro-preset1+ #x2b3)
(defconstant +key-macro-preset2+ #x2b4)
(defconstant +key-macro-preset3+ #x2b5)

;; Some keyboards have a buildin LCD panel where the contents are controlled
;; by the host. Often these have a number of keys directly below the LCD
;; intended for controlling a menu shown on the LCD. These keys often don't
;; have any labeling so we just name them KEY-KBD-LCD-MENU#
(defconstant +key-kbd-lcd-menu1+ #x2b8)
(defconstant +key-kbd-lcd-menu2+ #x2b9)
(defconstant +key-kbd-lcd-menu3+ #x2ba)
(defconstant +key-kbd-lcd-menu4+ #x2bb)
(defconstant +key-kbd-lcd-menu5+ #x2bc)

(defconstant +btn-trigger-happy+ #x2c0)
(defconstant +btn-trigger-happy1+ #x2c0)
(defconstant +btn-trigger-happy2+ #x2c1)
(defconstant +btn-trigger-happy3+ #x2c2)
(defconstant +btn-trigger-happy4+ #x2c3)
(defconstant +btn-trigger-happy5+ #x2c4)
(defconstant +btn-trigger-happy6+ #x2c5)
(defconstant +btn-trigger-happy7+ #x2c6)
(defconstant +btn-trigger-happy8+ #x2c7)
(defconstant +btn-trigger-happy9+ #x2c8)
(defconstant +btn-trigger-happy10+ #x2c9)
(defconstant +btn-trigger-happy11+ #x2ca)
(defconstant +btn-trigger-happy12+ #x2cb)
(defconstant +btn-trigger-happy13+ #x2cc)
(defconstant +btn-trigger-happy14+ #x2cd)
(defconstant +btn-trigger-happy15+ #x2ce)
(defconstant +btn-trigger-happy16+ #x2cf)
(defconstant +btn-trigger-happy17+ #x2d0)
(defconstant +btn-trigger-happy18+ #x2d1)
(defconstant +btn-trigger-happy19+ #x2d2)
(defconstant +btn-trigger-happy20+ #x2d3)
(defconstant +btn-trigger-happy21+ #x2d4)
(defconstant +btn-trigger-happy22+ #x2d5)
(defconstant +btn-trigger-happy23+ #x2d6)
(defconstant +btn-trigger-happy24+ #x2d7)
(defconstant +btn-trigger-happy25+ #x2d8)
(defconstant +btn-trigger-happy26+ #x2d9)
(defconstant +btn-trigger-happy27+ #x2da)
(defconstant +btn-trigger-happy28+ #x2db)
(defconstant +btn-trigger-happy29+ #x2dc)
(defconstant +btn-trigger-happy30+ #x2dd)
(defconstant +btn-trigger-happy31+ #x2de)
(defconstant +btn-trigger-happy32+ #x2df)
(defconstant +btn-trigger-happy33+ #x2e0)
(defconstant +btn-trigger-happy34+ #x2e1)
(defconstant +btn-trigger-happy35+ #x2e2)
(defconstant +btn-trigger-happy36+ #x2e3)
(defconstant +btn-trigger-happy37+ #x2e4)
(defconstant +btn-trigger-happy38+ #x2e5)
(defconstant +btn-trigger-happy39+ #x2e6)
(defconstant +btn-trigger-happy40+ #x2e7)

;; We avoid low common keys in module aliases so they don't get huge.
(defconstant +key-min-interesting+ +key-mute+)
(defconstant +key-max+ #x2ff)
(defconstant +key-cnt+ (1+ +key-max+))

;; Relative axes

(defconstant +rel-x+ #x00)
(defconstant +rel-y+ #x01)
(defconstant +rel-z+ #x02)
(defconstant +rel-rx+ #x03)
(defconstant +rel-ry+ #x04)
(defconstant +rel-rz+ #x05)
(defconstant +rel-hwheel+ #x06)
(defconstant +rel-dial+ #x07)
(defconstant +rel-wheel+ #x08)
(defconstant +rel-misc+ #x09)
;; #x0a is reserved and should not be used in input drivers.
;; It was used by HID as REL-MISC+1 and userspace needs to detect if
;; the next REL-* event is correct or is just REL-MISC + n.
;; We define here REL-RESERVED so userspace can rely on it and detect
;; the situation described above.
(defconstant +rel-reserved+ #x0a)
(defconstant +rel-wheel-hi-res+ #x0b)
(defconstant +rel-hwheel-hi-res+ #x0c)
(defconstant +rel-max+ #x0f)
(defconstant +rel-cnt+ (1+ +rel-max+))

;; Absolute axes

(defconstant +abs-x+ #x00)
(defconstant +abs-y+ #x01)
(defconstant +abs-z+ #x02)
(defconstant +abs-rx+ #x03)
(defconstant +abs-ry+ #x04)
(defconstant +abs-rz+ #x05)
(defconstant +abs-throttle+ #x06)
(defconstant +abs-rudder+ #x07)
(defconstant +abs-wheel+ #x08)
(defconstant +abs-gas+ #x09)
(defconstant +abs-brake+ #x0a)
(defconstant +abs-hat0x+ #x10)
(defconstant +abs-hat0y+ #x11)
(defconstant +abs-hat1x+ #x12)
(defconstant +abs-hat1y+ #x13)
(defconstant +abs-hat2x+ #x14)
(defconstant +abs-hat2y+ #x15)
(defconstant +abs-hat3x+ #x16)
(defconstant +abs-hat3y+ #x17)
(defconstant +abs-pressure+ #x18)
(defconstant +abs-distance+ #x19)
(defconstant +abs-tilt-x+ #x1a)
(defconstant +abs-tilt-y+ #x1b)
(defconstant +abs-tool-width+ #x1c)

(defconstant +abs-volume+ #x20)

(defconstant +abs-misc+ #x28)

;; #x2e is reserved and should not be used in input drivers.
;; It was used by HID as (+ 6 +abs-misc+) and userspace needs to detect if
;; the next ABS-* event is correct or is just (+ n +abc-misc).
;; We define here +abs-reserved+ so userspace can rely on it and detect
;; the situation described above.
(defconstant +abs-reserved+ #x2e)

(defconstant +abs-mt-slot+ #x2f
  "MT slot being modified")
(defconstant +abs-mt-touch-major+ #x30
  "Major axis of touching ellipse")
(defconstant +abs-mt-touch-minor+ #x31
  "Minor axis (omit if circular)")
(defconstant +abs-mt-width-major+ #x32
  "Major axis of approaching ellipse")
(defconstant +abs-mt-width-minor+ #x33
  "Minor axis (omit if circular)")
(defconstant +abs-mt-orientation+ #x34
  "Ellipse orientation")
(defconstant +abs-mt-position-x+ #x35
  "Center X touch position")
(defconstant +abs-mt-position-y+ #x36
  "Center Y touch position")
(defconstant +abs-mt-tool-type+ #x37
  "Type of touching device")
(defconstant +abs-mt-blob-id+ #x38
  "Group a set of packets as a blob")
(defconstant +abs-mt-tracking-id+ #x39
  "Unique ID of initiated contact")
(defconstant +abs-mt-pressure+ #x3a
  "Pressure on contact area")
(defconstant +abs-mt-distance+ #x3b
  "Contact hover distance")
(defconstant +abs-mt-tool-x+ #x3c
  "Center X tool position")
(defconstant +abs-mt-tool-y+ #x3d
  "Center Y tool position")

(defconstant +abs-max+ #x3f)
(defconstant +abs-cnt+ (1+ +abs-max+))

;; Switch events

(defconstant +sw-lid+ #x00
  "set = lid shut")
(defconstant +sw-tablet-mode+ #x01
  "set = tablet mode")
(defconstant +sw-headphone-insert+ #x02
  "set = inserted")
(defconstant +sw-rfkill-all+ #x03
  "rfkill master switch, type \"any\" set = radio enabled */")
(defconstant +sw-radio+ +sw-rfkill-all+
  "deprecated")
(defconstant +sw-microphone-insert+ #x04
  "set = inserted")
(defconstant +sw-dock+ #x05
  "set = plugged into dock")
(defconstant +sw-lineout-insert+ #x06
  "set = inserted")
(defconstant +sw-jack-physical-insert+ #x07
  "set = mechanical switch set")
(defconstant +sw-videoout-insert+ #x08
  "set = inserted")
(defconstant +sw-camera-lens-cover+ #x09
  "set = lens covered")
(defconstant +sw-keypad-slide+ #x0a
  "set = keypad slide out")
(defconstant +sw-front-proximity+ #x0b
  "set = front proximity sensor active")
(defconstant +sw-rotate-lock+ #x0c
  "set = rotate locked/disabled")
(defconstant +sw-linein-insert+ #x0d
  "set = inserted")
(defconstant +sw-mute-device+ #x0e
  "set = device disabled")
(defconstant +sw-pen-inserted+ #x0f
  "set = pen inserted")
(defconstant +sw-max+ #x0f)
(defconstant +sw-cnt+ (1+ +sw-max+))

;; Misc events

(defconstant +msc-serial+ #x00)
(defconstant +msc-pulseled+ #x01)
(defconstant +msc-gesture+ #x02)
(defconstant +msc-raw+ #x03)
(defconstant +msc-scan+ #x04)
(defconstant +msc-timestamp+ #x05)
(defconstant +msc-max+ #x07)
(defconstant +msc-cnt+ (1+ +msc-max+))

;; LEDs

(defconstant +led-numl+ #x00)
(defconstant +led-capsl+ #x01)
(defconstant +led-scrolll+ #x02)
(defconstant +led-compose+ #x03)
(defconstant +led-kana+ #x04)
(defconstant +led-sleep+ #x05)
(defconstant +led-suspend+ #x06)
(defconstant +led-mute+ #x07)
(defconstant +led-misc+ #x08)
(defconstant +led-mail+ #x09)
(defconstant +led-charging+ #x0a)
(defconstant +led-max+ #x0f)
(defconstant +led-cnt+ (1+ +led-max+))

;; Autorepeat values

(defconstant +rep-delay+ #x00)
(defconstant +rep-period+ #x01)
(defconstant +rep-max+ #x01)
(defconstant +rep-cnt+ (1+ +rep-max+))

;; Sounds

(defconstant +snd-click+ #x00)
(defconstant +snd-bell+ #x01)
(defconstant +snd-tone+ #x02)
(defconstant +snd-max+ #x07)
(defconstant +snd-cnt+ (1+ +snd-max+))
