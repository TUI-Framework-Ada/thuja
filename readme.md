# Thuja 🌲

Thuja is a **Text-Based User Interface (TUI) framework written in Ada**.  
It enables developers to build **interactive, modular, and high-integrity terminal applications** without manually handling rendering logic, layout calculations, or ANSI escape sequences.

Thuja was developed as part of **SWENG 480 – Software Engineering Design (Capstone)** at Penn State Behrend by **Team 26**, in collaboration with an industry client from **AdaCore**.

---

## ✨ Features

- **Entity–Component–System (ECS) Architecture**
  - Entities are lightweight identifiers
  - Components store widget state
  - Systems handle rendering, layout, and behavior

- **Reusable Widgets**
  - Progress bars, sliders, text areas, and more
  - Composed via ECS instead of inheritance

- **Flexbox-Inspired Layout System**
  - Constraint-based layout adapted for terminal environments
  - Supports justify, align, grow, shrink, and parent/child hierarchies

- **Efficient Rendering**
  - Pixel-level diffing ensures only modified characters are redrawn
  - Double-buffered to avoid full-screen flicker

- **ANSI Text Styling**
  - Full RGB foreground/background color via ANSI escape codes
  - Bold, italic, underline, and strikethrough support

- **Cross-Platform**
  - Consistent behavior on **Linux** and **Windows**

- **Developer Extensibility**
  - Standardized tab interface for building custom demo tabs
  - Custom widgets and systems can be integrated with minimal boilerplate

---

## 🎯 Project Goals

Thuja aims to:
- Expand the Ada ecosystem with a modern TUI framework
- Support safety-critical and high-integrity software development
- Provide predictable rendering and structured UI state management
- Demonstrate modern architectural patterns (ECS) in a strongly typed language

---

## 🧱 Architecture Overview

Thuja uses a **pure Entity–Component–System (ECS) architecture**:

- **Entities** - Unique identifiers with no embedded logic or state
- **Components** - Store widget data such as text, layout, visibility, and styling
- **Systems** - Operate over components to perform rendering, layout, and behavior updates

This design avoids deep inheritance hierarchies and simplifies widget layering, redraw ordering, state management, and extensibility.

---

## 🎮 Demos

The `demos/` directory contains a full interactive demo application with four tabs:

| Tab | Description |
|-----|-------------|
| **HTop** | Live system monitor showing CPU, memory, disk, and process stats |
| **Text Editor** | Modal text editor with navigation and insert modes |
| **Flexbox** | Interactive flexbox layout demo cycle justify/align, resize container |
| **Sort Visual** | Visual sorting algorithm demo with multiple algorithms |

### Running the Demo

```bash
cd demos
alr build
./bin/thuja_demo
```

### Demo Controls

| Key | Action |
|-----|--------|
| `[` / `]` | Switch tabs |
| `Esc` | Quit |

**HTop tab:**

| Key | Action |
|-----|--------|
| `b` | Toggle widget backgrounds |
| `p` | Toggle process list |

**Text Editor tab:**

| Key | Action |
|-----|--------|
| `i` | Enter insert mode |
| `Esc` | Return to navigation mode |
| `w` / `a` / `s` / `d` | Move cursor left / down / up / right |
| `Enter` | New line (insert mode) |
| `Tab` | Insert tab (insert mode) |
| `Backspace` | Delete character (insert mode) |

**Flexbox tab:**

| Key | Action |
|-----|--------|
| `j` | Cycle justify |
| `a` | Cycle align |
| `+` / `-` | Resize width |
| `H` / `h` | Resize height |

**Sort Visual tab:**

| Key | Action |
|-----|--------|
| `1` – `6` | Switch sorting algorithm |
| `Space` | Play / pause |
| `N` | Step forward one frame |
| `+` | Decrease speed |
| `-` | Increase speed |
| `R` | Reset |

---

## 🚀 Setup

### Prerequisites
- [Alire](https://alire.ada.dev/) package manager (`alr`)
  - Alire will automatically install the required GNAT toolchain

### Build the Library

```bash
alr build
```

### Build and Run the Demos

```bash
cd demos
alr build
./bin/thuja_demo
```

## 📂 Repository Structure

```
thuja/
├── src/                          # Core library
│   ├── ecs.ads / ecs.adb         # Entity Component System
│   ├── components.ads            # Component type definitions
│   ├── graphics.ads / .adb       # Buffer, pixel, and ANSI rendering
│   ├── flexbox.ads / .adb        # Flexbox layout algorithm
│   ├── input_handling.ads / .adb # Keyboard input
│   ├── ids.ads / .adb            # Entity and component ID types
│   ├── scroll.ads / .adb         # Scroll behavior
│   ├── selection.ads / .adb      # Focus and selection system
│   ├── system_stats.ads / .adb   # Cross-platform system stats (CPU, mem, disk)
│   ├── text_editor.ads / .adb    # Modal text editor logic
│   ├── htop.ads / .adb           # HTop data fetching and formatting
│   ├── standardized_tab_interface.ads  # Abstract tab interface
│   ├── linux/                    # Linux platform implementation
│   └── windows/                  # Windows platform implementation
│
├── demos/                        # Demo application
│   ├── src/                      # Demo-specific source
│   │   ├── thuja_demo.adb        # Main demo entry point
│   │   ├── thuja_demo_tab_htop   # HTop tab implementation
│   │   ├── thuja_demo_tab_editor # Text editor tab implementation
│   │   ├── thuja_demo_tab_flex   # Flexbox demo tab implementation
│   │   ├── thuja_demo_tab_sort   # Sort visualizer tab implementation
│   │   ├── flex_demo.ads / .adb  # Flexbox demo state and logic
│   │   └── sort_demo.ads / .adb  # Sort demo state and logic
│   ├── bin/                      # Compiled demo binaries
│   └── demos.gpr                 # Demo project file
│
├── tests/                        # Test suites
├── alire.toml                    # Alire package manifest
└── thuja.gpr                     # Library project file
```

---

## 👥 Team

**Team 26 - Penn State Behrend, SWENG 480**

- Jacob Norgard, David Gonzalez, Skye Stout, and Sargis Poghosyan
- Faculty Adviser: Dr. Ibrahim
- Industry Mentor: Olivier Henley
- Sponsored by: AdaCore
