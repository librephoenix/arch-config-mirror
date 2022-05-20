Config { font = "xft:UbuntuMono-R:size=16"
       , additionalFonts = ["xft:Symbols Nerd Font:size=16","xft:Bedstead for Powerline:size=21"]
       , borderColor = "#222222"
       , border = FullB
       , bgColor = "#282828"
       , fgColor = "#458588"
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
                    , Run Date "<fn=2><fc=#3b3838,#282828>\xe0b2</fc></fn><fc=#b16286,#3b3838> <fn=1>\xf073</fn> %a %-m/%-d/%y %-I:%M:%S%P </fc><fn=2><fc=#3b3838,#282828>\xe0b0</fc></fn> " "date" 10
                    , Run BatteryP ["BAT0"]
                      ["-t", "<acstatus>",
                      "-L", "10", "-H", "80", "-p", "3", "--",
                      "-O","<fc=#282828,#d79921> <fn=1>\xe61f</fn>+<fn=1>\xf303</fn></fc><fn=2><fc=#d79921,#3b3838>\xe0b0</fc></fn><fc=#98971a,#3b3838><fn=1>\xf583</fn><left>%</fc><fn=2><fc=#3b3838,#282828>\xe0b0</fc></fn>",
                      "-i","<fc=#282828,#d79921> <fn=1>\xe61f</fn>+<fn=1>\xf303</fn></fc><fn=2><fc=#d79921,#3b3838>\xe0b0</fc></fn><fc=#98971a,#3b3838><fn=1>\xf578</fn><left>%</fc><fn=2><fc=#3b3838,#282828>\xe0b0</fc></fn>",
                      "-o","<fc=#282828,#d79921> <fn=1>\xe61f</fn>+<fn=1>\xf303</fn></fc><fn=2><fc=#d79921,#3b3838>\xe0b0</fc></fn><fc=#cc241d,#3b3838><fn=1>\xf58b</fn><left>%</fc><fn=2><fc=#3b3838,#282828>\xe0b0</fc></fn>",
                      "-L", "-15", "-H", "-5",
                      "-l", "#cc241d", "-m", "#458588", "-h", "#98971z"] 10
                    , Run Brightness
                      [ "-t", "<fc=#d79921><fn=1>\xf5dd</fn> <percent>%</fc><fn=2><fc=#282828,#3b3838>\xe0b0</fc></fn>", "--",
                        "-D", "intel_backlight"
                      ] 2 
                    , Run Volume "default" "Master"
                      [ "-t", "<status>", "--"
                      , "--on", "<fc=#689d6a,#3b3838> <fn=1>\xf028</fn> <volume>%</fc><fn=2><fc=#3b3838,#282828>\xe0b0</fc></fn>"
                      , "--onc", "#689d6a"
                      , "--off", "<fc=#b16286,#3b3838> <fn=1>\xf026</fn>Mute</fc><fn=2><fc=#3b3838,#282828>\xe0b0</fc></fn>"
                      , "--offc", "#b16286"
                      ] 1 
                    , Run Com "/home/emmet/.config/xmobar/padding-icon.sh" [] "trayerpad" 2
                    , Run Mail [("<fn=1><fc=#458588>\xf6ed</fc></fn> ", "~/.mail/INBOX")] "mail"
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%battery% %bright%<action=`pavucontrol`>%default:Master%</action> %mail% }<action=`/home/emmet/.config/xmobar/open-org-calendar.sh`>%date%</action>{<box color=#3b3838 width=3>%UnsafeStdinReader%</box> %trayerpad%"
       }
}
