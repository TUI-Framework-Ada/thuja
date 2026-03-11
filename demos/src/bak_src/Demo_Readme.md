# Thuja TUI Framework - Demo Guide

## New Features Demonstrated

This package includes demos showcasing the latest features added to the Thuja terminal UI framework:

### 1. **Terminal Resize Detection** (`TerminalResizeSystem`)
- Automatically detects when the terminal window is resized
- Triggers layout recalculation for all Flexbox containers
- Ensures UI adapts dynamically to new dimensions

### 2. **Widget Movement API**
- `Move_Widget`: Absolute positioning (move widget to specific coordinates)
- `Move_Widget_By`: Relative positioning (move widget by delta)
- Automatically sets widgets to `Absolute` positioning mode

### 3. **Position Modes**
- **Flex**: Widget is positioned by FlexLayoutSystem (default)
- **Absolute**: Widget is manually positioned and ignored by Flexbox
- **Relative**: (Reserved for future use)
- **Fixed**: (Reserved for future use)

### 4. **Progress Bars** (`ProgressBarRenderSystem`)
- Animated progress indicators
- Customizable characters, colors, and borders
- Optional percentage display
- Automatically sized based on widget width

### 5. **Enhanced Flexbox**
- Column and Row directions
- Flex-grow, flex-shrink, flex-basis support
- Justify-content: Flex_Start, Center, Space_Between
- Align-items: Flex_Start, Center, Stretch

---

## Demo Files

### `comprehensive_demo.adb` - Full Feature Showcase

**What it demonstrates:**
- All 5 new features working together
- Multi-threaded rendering (30 FPS)
- Complex layout with header, sidebar, content area, and floating widget

**Layout:**
```
┌────────────────────────────────────────────┐
│ HEADER (Blue, Flex positioned)            │
├────────────────────────────────────────────┤
│ [████████       ] 75%  (Progress Bar)      │
├────────────────────────────────────────────┤
│ SIDEBAR  │ CONTENT AREA                    │
│ (Green)  │ (Red, with moving yellow dot)   │
└────────────────────────────────────────────┘
```

**Key Points:**
- Header, sidebar, content use **Flex positioning**
- Progress bar animates from 0% to 100%
- Yellow dot uses **Absolute positioning** and bounces around
- Terminal resize triggers automatic layout recalculation

**How to run:**
```bash
alr build
alr run comprehensive_demo
```

**What to try:**
1. Watch the progress bar fill up
2. Watch the yellow dot bounce around independently
3. Resize your terminal window - layout adapts automatically!

---

### `resize_and_movement_demo.adb` - Simplified Demo

**What it demonstrates:**
- Terminal resize detection
- Widget movement API
- Absolute positioning

**Layout:**
```
┌────────────────────────────────────────────┐
│                                            │
│        ┌───┐                               │
│        │BOX│  <- Bounces around            │
│        └───┘                               │
│                                            │
└────────────────────────────────────────────┘
```

**Key Points:**
- Single red box with "BOX" text
- Moves using `Move_Widget` API
- Bounces off screen edges
- Adapts when you resize the terminal

**How to run:**
```bash
alr build
alr run resize_and_movement_demo
```

**What to try:**
1. Watch the box bounce
2. Resize your terminal - the box adapts to new boundaries

---

## Code Structure

### Systems Execution Order (Main Loop)

```ada
-- 1. DETECT RESIZE (NEW!)
ECS.TerminalResizeSystem (Entity_List.all);

-- 2. CALCULATE LAYOUT
ECS.FlexLayoutSystem (Entity_List.all);

-- 3. RENDER BACKGROUNDS
ECS.WidgetBackgroundSystem (Entities_PO);

-- 4. RENDER TEXT
ECS.TextRenderSystem (Entities_PO);

-- 5. RENDER PROGRESS BARS (NEW!)
ECS.ProgressBarRenderSystem (Entities_PO);

-- 6. COPY TO FRAMEBUFFER
ECS.BufferCopySystem (Entities_PO);

-- 7. SWAP BUFFERS
ECS.DoubleBufferFlagSystem (Entities_PO);
```

### Using the Movement API

```ada
-- Absolute positioning (move to specific coordinates)
ECS.Move_Widget (Entity_List, Widget_ID, New_X => 40, New_Y => 12);

-- Relative movement (move by delta)
ECS.Move_Widget_By (Entity_List, Widget_ID, Delta_X => 5, Delta_Y => -3);
```

### Setting Position Modes

```ada
-- Flex positioning (controlled by FlexLayoutSystem)
Comp_PositionMode : constant Components.Position_Mode_Component_T := (
   Mode => Components.Flex
);

-- Absolute positioning (manual control, ignored by Flexbox)
Comp_PositionMode : constant Components.Position_Mode_Component_T := (
   Mode => Components.Absolute
);
```

---

## Implementation Notes

### `'Unchecked_Access` Usage

The code uses `'Unchecked_Access` in a few places:

1. **Protected object fields** (`Entities`, `Framebuffer_1`, `Framebuffer_2`)
   - These objects live inside protected types or components
   - They outlive the pointers that reference them
   - **Safe** because the parent object has appropriate lifetime

2. **What it means:**
   - Bypasses Ada's strict lifetime checking
   - You're responsible for ensuring objects don't get destroyed while pointers exist
   - In this codebase: objects are owned by the ECS and live for the program's duration

### Thread Safety

- **Protected objects** (`Entity_Components_PO`) ensure thread-safe access
- **Claim_Reading**: Multiple readers can access simultaneously
- **Claim_Writing**: Exclusive write access
- **BufferDrawSystem** runs in separate thread at 30 FPS

---

## Troubleshooting

**Issue:** Widgets don't move
- **Check:** Is the widget's `Position_Mode` set to `Absolute`?
- Flex-positioned widgets are controlled by FlexLayoutSystem

**Issue:** Layout doesn't resize
- **Check:** Is `TerminalResizeSystem` called before `FlexLayoutSystem`?
- **Check:** Are Flexbox containers marked `Is_Dirty => True` initially?

**Issue:** Progress bar doesn't show
- **Check:** Is `ProgressBarRenderSystem` called after `WidgetBackgroundSystem`?
- **Check:** Does the widget have both `WidgetComponent` and `ProgressBarComponent`?

**Issue:** Compilation warnings about "unprotected access"
- These are **warnings**, not errors
- They indicate use of `'Unchecked_Access` on protected object fields
- Safe in this codebase due to proper lifetime management

---

## Next Steps

Try modifying the demos:

1. **Add more widgets** to the comprehensive demo
2. **Change Flexbox settings** (Direction, Justify, Align)
3. **Add text to moving widgets**
4. **Create multiple bouncing objects**
5. **Implement collision detection** between widgets

Have fun building terminal UIs with Thuja!
