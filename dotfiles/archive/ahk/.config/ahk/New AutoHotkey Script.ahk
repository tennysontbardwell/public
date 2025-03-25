#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
#Persistent
#SingleInstance Force

; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.

Clips = ""
First = 1
Mine = 0
OnChange() {
	global Clips
	global Mine
	global First
	if (Mine) {
		Mine = 0
	} else {
		If(First == 1) {
			Clips = %clipboard%
			First = 0
		} else {
			Clips = %Clips% %clipboard%
		}
		Clips2 = %clipboard%
		clipboard = %Clips%
		Mine = 1
	}
}

OnClipboardChange("OnChange")