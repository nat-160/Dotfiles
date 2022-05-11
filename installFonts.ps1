# WARNING! code is barely functional


# Download Fonts to Temp
$fPath = Join-Path ([System.IO.Path]::GetTempPath()) 'fonts'
$fZip = New-TemporaryFile
Invoke-WebRequest 'https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/FiraCode.zip' -OutFile $fZip
Expand-Archive $fZip $fPath

# Invoke the system font installer
$fonts = Get-ChildItem $fPath Fira*Complete.ttf
foreach($f in ($fonts)){
  Invoke-Item $f
  Pause
}
