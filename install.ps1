# Installs the latest cert-prep release for Windows x86_64.
# Usage: powershell -ExecutionPolicy Bypass -c "irm https://raw.githubusercontent.com/waveFrontSet/cert-prep/main/install.ps1 | iex"
$ErrorActionPreference = "Stop"

$Repo = "waveFrontSet/cert-prep"
$Asset = "cert-prep-windows-x86_64.zip"
$InstallDir = if ($env:CERT_PREP_INSTALL_DIR) { $env:CERT_PREP_INSTALL_DIR }
              else { Join-Path $env:LOCALAPPDATA "Programs\cert-prep" }

$Url = "https://github.com/$Repo/releases/latest/download/$Asset"
$Tmp = Join-Path $env:TEMP $Asset

Write-Host "Downloading $Url"
Invoke-WebRequest -Uri $Url -OutFile $Tmp

New-Item -ItemType Directory -Force -Path $InstallDir | Out-Null
Expand-Archive -Path $Tmp -DestinationPath $InstallDir -Force
Remove-Item $Tmp

Write-Host "Installed cert-prep.exe to $InstallDir"
if (($env:Path -split ";") -notcontains $InstallDir) {
    Write-Host "Note: $InstallDir is not on your PATH. Add it with:"
    Write-Host "  [Environment]::SetEnvironmentVariable('Path', ([Environment]::GetEnvironmentVariable('Path', 'User') + ';$InstallDir'), 'User')"
    Write-Host "Then restart your terminal."
}
