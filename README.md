# ASCII Generator

A lightweight, fast utility that converts images, video frames, or text into customizable ASCII art right in your terminal or output files.

---

## Features

* **Image to ASCII**: Convert `.jpg`, `.png`, `.bmp`, and `.webp` images into stylized text art.
* **Custom Character Sets**: Choose between standard and higher detail drawing.
* **Color Support**: Output in colour.
* **Custom Dimensions**: Automatically scale output to fit terminal size or explicit width/height constraints with a choice of downsampling algorithms.

---

## Installation

### Prerequisites

You need [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/) and [Stack](https://docs.haskellstack.org/en/stable/README/) installed on your machine. The easiest way to set up the Haskell toolchain is via `ghcup`:

```bash
# Install GHCup (GHC, Stack, and Cabal)
curl --proto '=https' --tlsv1.2 -sSf [https://get-ghcup.haskell.org](https://get-ghcup.haskell.org) | sh
```

```bash
# Clone the repository
git clone https://github.com/gabigoranov/ascii-generator.git

# Navigate to project directory
cd ascii-generator

# Setup compiler and dependencies
stack setup

# Build the project
stack build

# Install the binary globally (optional)
stack install
```

---

## Quick Start

### Basic Image Conversion

Convert an image and print the output directly to the terminal:

```bash
stack run

# Enter path to image as prompted.
# Enter algorithm of choice and desired output size as prompted.
```

---

## Project Structure

```text
ascii-generator/
├── app/               # Application entry point
├── src/               # Core library source files
│   ├── Ascii/         # Submodules for ASCII rendering logic
│   ├── InputHelper.hs # CLI argument parsing and input handling
│   └── Lib.hs          # Main library exports
├── test/              # Test suite
├── package.yaml       # Project dependencies and metadata
├── stack.yaml         # Stack build configuration
└── README.md
```

---

## Known Limitations

The project is actively under development. The following features are currently work-in-progress or planned for upcoming releases:

- **Custom Configuration**: Customizing font styles, character ramps, and color outputs is not yet supported.
- **CLI Image Pathing**: Passing image paths or additional arguments via `stack run --path` is not fully wired up yet.
- **Cross-Platform Pre-built Binaries**: Native standalone executable builds for Windows, GNU/Linux, and macOS are not yet provided in releases.
- **Test Coverage**: Comprehensive unit test suites and integration tests are currently being written.
- **Exporting Capabilities**: Direct file saving or exporting of the generated ASCII output is not yet implemented.

---

## Contributing

Contributions are welcome! Please follow these steps:

1. Fork the repository.
2. Create your feature branch (`git checkout -b feature/AmazingFeature`).
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`).
4. Push to the branch (`git push origin feature/AmazingFeature`).
5. Open a Pull Request.

---

## License

Distributed under the BSD 3-Clause License. See `LICENSE` for more information.
