local wezterm = require 'wezterm';

function basename(s)
  return string.gsub(s, "(.*[/\\])(.*)", "%2")
end

function log_proc(proc, indent)
  indent = indent or ""
  wezterm.log_info(indent .. "pid=" .. proc.pid .. ", name=" .. proc.name .. ", status=" .. proc.status)
  wezterm.log_info(indent .. "argv=" .. table.concat(proc.argv, " "))
  wezterm.log_info(indent .. "executable=" .. proc.executable .. ", cwd=" .. proc.cwd)
  for pid, child in pairs(proc.children) do
    log_proc(child, indent .. "  ")
  end
end

wezterm.on("mux-is-process-stateful", function(proc)
  log_proc(proc)

  -- Just use the default behavior
  return nil
end)

wezterm.on(
  "nat-debug", function(window, pane)
    wezterm.log_info(pane:get_foreground_process_name())
    wezterm.log_info(pane:pane_id())
end)

wezterm.on(
  "format-tab-title", function(tab, tabs, panes, config, hover, max_width)
    local pane = tab.active_pane
    local title = basename(pane.foreground_process_name)
    return {{Text=" " .. title .. " "}}
end)

wezterm.on(
  "update-right-status", function(window, pane)
    local title = basename(pane:get_foreground_process_name())
    local ktable = window:active_key_table()
    if title == "pwsh" then
      if not ktable then
        window:perform_action(wezterm.action{ActivateKeyTable={name="pwsh_compatibility"}}, pane)
      end
    else
      if ktable == "pwsh_compatibility" then
        window:perform_action("ClearKeyTableStack", pane)
      end
    end
    window:set_right_status(title .. " - " .. (ktable or "default"))
end)

wezterm.on(
  "nat-delete-other-pane", function(window, pane)
    hpane = pane:pane_id()
    window:perform_action(wezterm.action{ActivatePaneDirection="Next"},pane)
    window:perform_action(wezterm.action{EmitEvent="nat-try-kill-pane"},pane)
end)

wezterm.on(
  "nat-try-kill-pane", function(window, pane)
    local apane = pane:pane_id()
    if not (hpane == apane) then
      window:perform_action(wezterm.action{CloseCurrentPane={confirm=true}},pane)
    end
end)


return {
  -- Debug Mode
  debug_key_events = false,

  -- Visuals
  -- hide_tab_bar_if_only_one_tab = true,
  window_background_opacity = 0.9,
  color_scheme = "nord",
  font = wezterm.font_with_fallback({
      "FiraCode Nerd Font",
  }),

  -- Behaviour
  disable_default_key_bindings = true,
  default_prog = {"pwsh", "-nologo"},
  skip_close_confirmation_for_processes_named = {
    "bash", "sh", "pwsh"
  },

  -- Key Bindings
  key_tables = {
    pwsh_compatibility = {
      {key="h", mods="CTRL", action={SendKey={key="a", mods="CTRL|ALT"}}},
      {key="\t", mods="CTRL", action={SendKey={key="b", mods="CTRL|ALT"}}},
      {key="j", mods="CTRL", action={SendKey={key="c", mods="CTRL|ALT"}}},
      {key="m", mods="CTRL", action={SendKey={key="d", mods="CTRL|ALT"}}},
      {key="s", mods="CTRL", action={SendKey={key="e", mods="CTRL|ALT"}}},
      {key="Enter", mods="SHIFT", action={SendKey={key="f", mods="CTRL|ALT"}}},
      {key="=", mods="ALT", action={SendKey={key="+", mods="ALT"}}},
      {key="x", mods="CTRL", action={ActivateKeyTable={name="emacs_C_x", one_shot=true}}},
      {key="V", mods="CTRL", action={PasteFrom="Clipboard"}},
      {key="w", mods="ALT", action={CopyTo="Clipboard"}},
    },
    emacs_C_x = {
      {key="o", action={ActivatePaneDirection="Next"}},
      {key="0", action={CloseCurrentPane={confirm=true}}},
      {key="1", action={EmitEvent="nat-delete-other-pane"}},
      {key="2", action={SplitVertical={domain="CurrentPaneDomain"}}},
      {key="3", action={SplitHorizontal={domain="CurrentPaneDomain"}}},

      {key="c", mods="CTRL", action={CloseCurrentTab={confirm=true}}},
      {key="C", mods="CTRL", action={CloseCurrentTab={confirm=false}}},

      {key="u", action={
         Multiple={
           {SendKey={key="x", mods="CTRL"}},
           {SendKey={key="u", mods="CTRL"}},
         }
      }},

      {key="s", mods="CTRL", action={SendKey={key="x", mods="CTRL"}}}, --exit nano by "saving"
    },
  },

  keys = {
    {key="?", mods="SHIFT", action={EmitEvent="nat-debug"}},
  }
  --   {key="w", mods="ALT", action={CopyTo="Clipboard"}},
  --   {key="z", mods="CTRL", action="HideApplication"},
  --   {key="F11", mods="", action="ToggleFullScreen"},
  --   {key="v", mods="CTRL|SHIFT", action={PasteFrom="Clipboard"}},
}
