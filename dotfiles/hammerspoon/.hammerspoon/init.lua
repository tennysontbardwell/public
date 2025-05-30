--------------------------------------------------------------------------------
--- Plugins / Functions
--------------------------------------------------------------------------------

mouseCircle = nil
mouseCircleTimer = nil

function mouseHighlight()
    -- Delete an existing highlight if it exists
    if mouseCircle then
        mouseCircle:delete()
        if mouseCircleTimer then
            mouseCircleTimer:stop()
        end
    end
    -- Get the current co-ordinates of the mouse pointer
    mousepoint = hs.mouse.absolutePosition()
    -- Prepare a big red circle around the mouse pointer
    mouseCircle = hs.drawing.circle(hs.geometry.rect(mousepoint.x-40, mousepoint.y-40, 80, 80))
    mouseCircle:setStrokeColor({["red"]=1,["blue"]=0,["green"]=0,["alpha"]=1})
    mouseCircle:setFill(false)
    mouseCircle:setStrokeWidth(5)
    mouseCircle:show()

    -- Set a timer to delete the circle after 3 seconds
    mouseCircleTimer = hs.timer.doAfter(3, function()
      mouseCircle:delete()
      mouseCircle = nil
    end)
end

-- TODO https://www.hammerspoon.org/Spoons/RecursiveBinder.html
-- TODO https://www.hammerspoon.org/Spoons/Seal.html

--------------------------------------------------------------------------------
--- Helper Functions
--------------------------------------------------------------------------------

function defaultLeader(key, f)
  hs.hotkey.bind({"cmd", "ctrl"}, key, f)
end

function windowPercent(key, xSize, ySize, xOff, yOff)
  local xOff = xOff or 0
  local yOff = yOff or 0
  local xSize = xSize or (1-xOff)
  local ySize = ySize or (1-yOff)

  defaultLeader(key, function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    local xOff = xOff * max.w
    local yOff = yOff * max.h
    local xSize = xSize * max.w
    local ySize = ySize * max.h

    f.x = xOff
    f.y = yOff
    f.w = xSize
    f.h = ySize
    -- hs.alert.show(key .. '(' .. f.x .. ', ' .. f.y .. ') to (' .. f.x + f.w .. ', ' .. f.y + f.h .. ')')
    win:setFrame(f, 0)
  end)
end

function appKey(key, name)
  defaultLeader(key, function()
    local app = hs.application.find(name)
    if app then
      app:setFrontmost()
    else
      hs.alert.show("not found", hs.alert.defaultStyle, hs.screen.mainScreen(), 0.5)
    end
  end)
end

--------------------------------------------------------------------------------
--- Key Bindings
--------------------------------------------------------------------------------

appKey("F", "firefox")
appKey("T", "iterm2")
appKey("D", "emacs")
appKey("R", "rstudio")
appKey("S", "sioyek")

defaultLeader("I", function()
  hs.reload()
  hs.alert.show("Config loaded", hs.alert.defaultStyle, hs.screen.mainScreen(), 0.5)
end)

windowPercent("K")
windowPercent("C", 0.5, 0.5, 0.25, 0.25)
windowPercent("J", 0.5, nil)
windowPercent("L", nil, nil, 0.5, nil)
windowPercent("U", 0.4, nil)
windowPercent("O", nil, nil, 0.4, nil)

hs.loadSpoon("KSheet")
defaultLeader("N", function() spoon.KSheet:toggle() end)

-- defaultLeader("N", mouseHighlight)

hs.hotkey.bind({"shift", "cmd"}, "c", function()
  hs.timer.delayed.new(0.1, function()
    hs.eventtap.keyStroke({"cmd"}, "c")
    hs.execute("(pbpaste; echo '') >> /tmp/clipboard")
    hs.alert.show("Copied to /tmp/clipboard", hs.alert.defaultStyle, hs.screen.mainScreen(), 0.1)
  end).start()
end)

--------------------------------------------------------------------------------
--- Other Key Bindings
--------------------------------------------------------------------------------

-- keyboardTracker = hs.eventtap.new({ events.keyDown }, function (e)
--   local flags = e:getFlags()
--   local noMods = now (flags.cmd or flags.ctrl or flags.option or flags.alt or flags.fn)

--   if (e:getKeyCode() == hs.keycodes.map.j) and noMods then
--     local application = hs.application.frontmostApplication()
--     hs.alerts.show(application:bundleID())
--     if (application:bundleID() == "com.google.Chrome") then
--       if keyCode == hs.keycodes.map.f then
--         if not allowOverride then
--           return true
--         end
--         allowOverride = false
--       elseif keyCode == hs.keycodes.map.n then
--         hs.eventtap.event.newKeyEvent(hs.keycodes.map.cmd, true):post()
--         hs.eventtap.event.newKeyEvent("f", true):post()
--         hs.eventtap.event.newKeyEvent("f", false):post()
--         hs.eventtap.event.newKeyEvent(hs.keycodes.map.cmd, false):post()

--         allowOverride = true
--         return true
--       end
--     end
--   end
-- end)

--------------------------------------------------------------------------------
--- Menu Bar
--------------------------------------------------------------------------------

caffeine = hs.menubar.new()
function setCaffeineDisplay(state)
    if state then
        caffeine:setTitle(utf8.char(0x274C) .. utf8.char(0x1F4A4))
    else
        caffeine:setTitle(utf8.char(0x1F4A4))
    end
end

function caffeineClicked()
    setCaffeineDisplay(hs.caffeinate.toggle("displayIdle"))
end

if caffeine then
    caffeine:setClickCallback(caffeineClicked)
    setCaffeineDisplay(hs.caffeinate.get("displayIdle"))
end

hs.loadSpoon("ZeroOffset")
spoon.ZeroOffset:start()


function menuInfo(f)
  menuitem = hs.menubar.new()
  local function updateInfo()
    menuitem:setTitle(f())
    menuitem:setTooltip("Current Wattage")
  end
  hs.timer.doEvery(1, updateInfo)
  updateInfo()
end

menuInfo(function ()
  return hs.execute("system_profiler SPPowerDataType | grep Wattage | tr -d '\\n' | tail -c 3") .. "W"
end)


--------------------------------------------------------------------------------
--- End Matter
--------------------------------------------------------------------------------

hs.alert.show("Config loaded x", hs.alert.defaultStyle, hs.screen.mainScreen(), 0.5)
