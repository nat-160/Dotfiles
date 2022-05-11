local wezterm = require 'wezterm';

return {
  color_scheme = "nord",
  default_prog = {"pwsh", "-nologo"},
  font = wezterm.font_with_fallback({
    "FiraCode Nerd Font",
  }),
}
