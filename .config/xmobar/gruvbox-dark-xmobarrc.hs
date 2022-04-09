Config { font = "xft:UbuntuMono-R:size=14"
       , additionalFonts = ["xft:Symbols Nerd Font"]
       , borderColor = "#282828"
       , border = FullB
       , bgColor = "#282828"
       , fgColor = "#458588"
       , alpha = 225
       , position = TopSize C 100 30 
       , textOffset = -1
       , iconOffset = -1
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False 
       , hideOnStart = False
       , iconRoot = "."
       , allDesktops = True
       , overrideRedirect = True
       , commands = [
                      Run UnsafeStdinReader
                    , Run Date "<fc=#d79921>%a</fc> <fc=#b16286>%-m/%-d/%y</fc> <fc=#689d6a>%-I:%M:%S%P</fc>" "date" 10
                    , Run BatteryP ["BAT0"]
                      ["-t", "<acstatus>",
                      "-L", "10", "-H", "80", "-p", "3",
                      "--", "-O"," <fn=1><fc=#b16286>\xe61f</fc></fn> <fc=#cc241d>X</fc><fc=#d79921>m</fc><fc=#98971a>o</fc><fc=#689d6a>n</fc><fc=#458588>a</fc><fc=#b16286>d</fc> <fc=#cc241d>+</fc> <fn=1><fc=#d79921>\xf303</fc></fn> <fc=#98971a>A</fc><fc=#689d6a>r</fc><fc=#458588>c</fc><fc=#b16286>h</fc> <fc=#ebdbb2>|</fc> <fc=#98971a><fn=1>\xf583</fn><left>%</fc>",
                      "-i"," <fn=1><fc=#b16286>\xe61f</fc></fn> <fc=#cc241d>X</fc><fc=#d79921>m</fc><fc=#98971a>o</fc><fc=#689d6a>n</fc><fc=#458588>a</fc><fc=#b16286>d</fc> <fc=#cc241d>+</fc> <fn=1><fc=#d79921>\xf303</fc></fn> <fc=#98971a>A</fc><fc=#689d6a>r</fc><fc=#458588>c</fc><fc=#b16286>h</fc> <fc=#ebdbb2>|</fc> <fc=#98971a><fn=1>\xf578</fn><left>%</fc>",
                      "-o"," <fn=1><fc=#b16286>\xe61f</fc></fn> <fc=#cc241d>X</fc><fc=#d79921>m</fc><fc=#98971a>o</fc><fc=#689d6a>n</fc><fc=#458588>a</fc><fc=#b16286>d</fc> <fc=#cc241d>+</fc> <fn=1><fc=#d79921>\xf303</fc></fn> <fc=#98971a>A</fc><fc=#689d6a>r</fc><fc=#458588>c</fc><fc=#b16286>h</fc> <fc=#ebdbb2>|</fc> <fc=#cc241d><fn=1>\xf58b</fn><left>%</fc>",
                      "-L", "-15", "-H", "-5",
                      "-l", "#cc241d", "-m", "#458588", "-h", "#98971z"] 10
                    , Run Brightness
                      [ "-t", "<fc=#d79921><fn=1>\xf5dd</fn> <percent>%</fc>", "--",
                        "-D", "intel_backlight"
                      ] 2 
                    , Run Volume "default" "Master"
                      [ "-t", "<status>", "--"
                      , "--on", "<fc=#689d6a><fn=1>\xf028</fn> <volume>%</fc>"
                      , "--onc", "#689d6a"
                      , "--off", "<fc=#b16286><fn=1>\xf026</fn>Mute</fc>"
                      , "--offc", "#b16286"
                      ] 1 
                    , Run Com "/home/emmet/.config/xmobar/padding-icon.sh" [] "trayerpad" 2
                    , Run Mail [("<fn=1><fc=#458588>\xf6ed</fc></fn> ", "~/.mail/INBOX")] "mail"
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%battery% %bright% <action=`pavucontrol`>%default:Master%</action> %mail%}%UnsafeStdinReader%{<action=`/home/emmet/.config/xmobar/open-org-calendar.sh`>%date%</action> %trayerpad%"
       }
}
