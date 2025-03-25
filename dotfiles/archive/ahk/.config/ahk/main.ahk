#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
#SingleInstance Force

Post(url) {
	oHttp := ComObjCreate("WinHttp.Winhttprequest.5.1")
	oHttp.open("POST",url)
	oHttp.send()
}

!^+Space::
WinGet, active_id, ID, A
clipboard = %active_id%

!^+a:: WinActivate ahk_id 0x20404

!^+s:: WinActivate ahk_id 0xa0c52

!^+d:: WinActivate ahk_id 0x404da

!^+f:: WinActivate ahk_id 0x2200aca

!^+g:: WinActivate ahk_id 0x710642

!^+e:: WinActivate ahk_id 0x5e0b46

!^+b:: WinActivate ahk_id 0x7a0754

!^+l::
Run C:\Users\tenny\Desktop\main.ahk
return

;!^+Space::
::
Input, res, , {Esc}{Enter}, srw,slw,srh,slh,koj
switch res
{
Case "koj":
	Run kitty -load nyc1-arch-misc1
	return
Case "srw":
	Post("http://nyc1-arch-misc1.node.consul.tennysontbardwell.com:5000/api/v0/ra/screen/1-w")
	return
Case "srh":
	Post("http://nyc1-arch-misc1.node.consul.tennysontbardwell.com:5000/api/v0/ra/screen/1-h")
	return
Case "slw":
	Post("http://nyc1-arch-misc1.node.consul.tennysontbardwell.com:5000/api/v0/ra/screen/4-w")
	Sleep 1000
	Post("http://nyc1-arch-misc1.node.consul.tennysontbardwell.com:5000/api/v0/ra/screen/3-w")
	Sleep 1000
	Post("http://nyc1-arch-misc1.node.consul.tennysontbardwell.com:5000/api/v0/ra/screen/2-w")
	return
Case "slh":
	Post("http://nyc1-arch-misc1.node.consul.tennysontbardwell.com:5000/api/v0/ra/screen/4-h")
	Sleep 1000
	Post("http://nyc1-arch-misc1.node.consul.tennysontbardwell.com:5000/api/v0/ra/screen/3-h")
	Sleep 1000
	Post("http://nyc1-arch-misc1.node.consul.tennysontbardwell.com:5000/api/v0/ra/screen/2-h")

	return
Default:
	MsgBox, Macro not found %res%
	return
}


WinGet, ActiveId, ID, A
WinWaitNotActive, ahk_id %ActiveId%
WinActivate, ahk_id %ActiveId%

F1::
list = ""
Index = ""
Menu, windows, Add
Menu, windows, deleteAll
WinGet, id, list,,, Program Manager
Loop, %id%
{
this_ID := id%A_Index%
WinGet, exStyle, exStyle, ahk_id %this_ID%
  If !(exStyle & 0x100)
   Continue
WinGetTitle, title, ahk_id %this_ID%
  If (title = "")
   Continue
WinGetClass, class, ahk_id %this_ID%
 If (class = "")
   Continue
Index += 1
 If (class = "ApplicationFrameWindow")
  Menu, windows, Add, &%Index% %title%%A_Tab%ApplicationFrameWindow, ActivateWindow
 else
 {
   ;; Menu, windows, Add, &%Index% %title%%A_Tab%%class%, ActivateWindow
   WinGet, Path, ProcessPath, ahk_id %this_ID%
   ;; If (!Errorlevel)
   ;; Menu, windows, Icon, &%Index% %title%%A_Tab%%class%, %Path%
 }
}
CoordMode, Mouse, Screen
MouseMove, 100, 30
CoordMode, Menu, Screen
Menu, windows, Show, 0, 0
return

ActivateWindow:
StringTrimLeft, ThisMenuItem, A_ThisMenuItem, 3
StringReplace, ThisMenuItem, ThisMenuItem, %A_Tab%, %A_Space%ahk_class%A_Space%
SetTitleMatchMode, 3
WinActivate, %ThisMenuItem%
GoSub, MouseCenterInWindow
return

MouseCenterInWindow:
CoordMode, Mouse, Relative
WinGetPos,,,Xmax,Ymax,A ; get active window size
Xcenter := Xmax/2        ; calculate center of active window
Ycenter := Ymax/2
MouseMove, Xcenter, Ycenter
return