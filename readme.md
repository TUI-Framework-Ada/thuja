# Thuja 🌲

Thuja is a **Text-Based User Interface (TUI) framework written in Ada**.  
It enables developers to build **interactive, modular, and high-integrity terminal applications** without manually handling rendering logic, layout calculations, or ANSI escape sequences.

Thuja was developed as part of **SWENG 480 – Software Engineering Design (Capstone)** at Penn State by **Team 26**, in collaboration with an industry client from **AdaCore**.

---

## ✨ Features

- **Entity–Component–System (ECS) Architecture**
  - Entities are lightweight identifiers
  - Components store widget state
  - Systems handle rendering, layout, and behavior

- **Reusable Widgets**
  - Widgets such as sliders and progress bars
  - Widgets composed via ECS instead of inheritance

- **Flexbox-Inspired Layout System**
  - Constraint-based layout adapted for terminal environments
  - Supports parent/child widget hierarchies

- **Efficient Rendering**
  - Pixel-level diffing ensures only modified characters are redrawn
  - Avoids full-screen redraws

- **ANSI Text Styling**
  - Foreground color and text attributes using ANSI escape codes

- **Cross-Platform**
  - Designed for consistent behavior on **Linux** and **Windows**

- **Developer Extensibility**
  - Developers can create and integrate custom widgets

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

- **Entities**
  - Unique identifiers with no embedded logic or state

- **Components**
  - Store widget data such as text, layout, visibility, and styling

- **Systems**
  - Operate over components to perform rendering, layout calculations, and behavior updates

This design avoids deep inheritance hierarchies and simplifies:
- Widget layering
- Redraw ordering
- State management
- Extensibility

---

## 🧪 Testing

Thuja follows a **test-driven development approach**, with focused test suites covering:

- Custom widget instantiation
- Pixel-perfect redraw behavior
- Overlapping widget rendering and layering
- Parent/child bounds enforcement
- Full framework integration

---

## 🚀 Setup (Windows / Linux)

> 🚧 Setup instructions coming soon.
>
> This section will document required dependencies, compiler setup, and how to build and run demos on both Windows and Linux.

---

## 📂 Repository Structure

