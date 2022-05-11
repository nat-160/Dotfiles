<#
.SYNOPSIS
  Installs the fonts "FiraCode Nerd" and "SourceSansPro" from web
#>

# Define list of zipped fonts
$fontZips = @('https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/FiraCode.zip', 'https://www.fontsquirrel.com/fonts/download/source-sans-pro')

# Define folder to extract to
$fontPath = Join-Path ([System.IO.Path]::GetTempPath()) 'fonts'

# Download and extract files
foreach($fontZip in $fontZips){
  $tempZip = New-TemporaryFile
  Invoke-WebRequest $fontZip -OutFile $tempZip
  Expand-Archive $tempZip $fontPath
}

# Invoke the system font installer
$fontFiles = Get-ChildItem $fontPath "Fira*Complete.ttf"
$fontFiles+= Get-ChildItem $fontPath "SourceSansPro*.otf"
foreach($fontFile in ($fontFiles)){
  Invoke-Item $fontFile
  Pause
}
