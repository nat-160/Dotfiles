# Modules
$modules = "Posh-Git", "Terminal-Icons"
foreach ($m in $modules) {
  if (!(Import-Module $m -ErrorAction Ignore)) {
    Install-Module $m
    Import-Module $m
  }
}

# Emacs Keybindings
Set-PSReadLineOption -EditMode 1

# Compatibility Bindings
if ($env:TERM_PROGRAM -eq "WezTerm") {
  $_H = "Alt+a"
  $_I = "Alt+b"
  $_J = "Alt+c"
  $_M = "Alt+d"
  $_S = "Alt+e"
  $_ShiftEnter = "Ctrl+Alt+f"
  $_Eq = "+"
}
else {
  $_H = "H"
  $_I = "I"
  $_J = "J"
  $_M = "M"
  $_S = "S"
  $_ShiftEnter = "Shift+Enter"
  $_Eq = "="
}

# Override Defaults
Set-PSReadLineKeyHandler -Key Tab -Function MenuComplete
Set-PSReadLineKeyHandler -Key Shift+Tab -Function TabCompletePrevious
Set-PSReadLineKeyHandler -Key Ctrl+d -Function DeleteChar

# Wezterm Rebindings
Set-PSReadLineKeyHandler -Key $_ShiftEnter -Function InsertLineBelow
Set-PSReadLineKeyHandler -Key Alt+$_Eq -Function PossibleCompletions

# Added Functions
if ($isWindows) {
  Set-PSReadLineKeyHandler -Key Ctrl+v -Function ScrollDisplayDown
  Set-PSReadLineKeyHandler -Key Alt+v -Function ScrollDisplayUp
}
Set-PSReadLineKeyHandler -Key Alt+m -Function GotoFirstNonBlankOfLine
Set-PSReadLineKeyHandler -Chord "Ctrl+$_H,k" -Function WhatIsKey
Set-PSReadLineKeyHandler -Chord "Ctrl+$_H,b" -Function ShowKeyBindings

# Functions/Aliases
function Invoke-Sudo {
  if ($args) {
    $output = sudo pwsh -NoProfile -OutputFormat XML -Command ($args -join " ")
    $output
  }
  else {
    sudo
  }
}
Set-Alias "ls" -Value "Get-ChildItem"
Set-Alias "isudo" -Value "Invoke-Sudo"

# Start Starship
Invoke-Expression (&starship init powershell)
