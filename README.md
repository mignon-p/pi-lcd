[![Hackage](https://img.shields.io/hackage/v/pi-lcd.svg)](https://hackage.haskell.org/package/pi-lcd)
[![Build Status](https://travis-ci.org/ppelleti/pi-lcd.svg?branch=master)](https://travis-ci.org/ppelleti/pi-lcd)

This package allows using an
[Adafruit character LCD and keypad kit][1] on a Raspberry Pi from
Haskell.  The LCD+Keypad Kit has a 16x2 character LCD, an RGB LED
backlight, and five buttons.  It works on all models of Raspberry Pi,
and plugs into the GPIO header.  It only uses the I²C pins on the
header.

This library automatically handles Unicode, using the LCD controller's
built-in characters where possible, and using the "custom characters"
feature, along with a [5x8 font][3] bundled with this package, to
emulate Unicode characters which are not built-in.  The user of the
library can also define their own custom characters.

Everything you need to use one of these LCDs is exposed in the
`System.Hardware.PiLcd` module.  The submodules are only exposed
because they might be useful to reuse for other types of hardware,
such as if you are using an MCP23017 port expander or an HD44780
display controller in some other context.  But if you're using the
Adafruit character LCD and keypad kit, all you need is
`System.Hardware.PiLcd`.

Before using this package, you'll need to make sure that
[I²C is enabled][2] on your Raspberry Pi, under "Advanced Options" in
`raspi-config`.

Using `PiLcd` is as easy as:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import System.Hardware.PiLcd

main = do
  lcd <- openPiLcd defaultLcdAddress defaultLcdOptions
  updateDisplay lcd ["Hello,", "World!"]
  setBacklightColor lcd Cyan
  closePiLcd lcd
```

There are additional examples in the `examples` directory.

This library is specifically designed to work with GHC 7.6.3 and up,
and to not use Template Haskell, so it will work with the
system-supplied version of GHC on Raspbian "Jessie."  Just do
`sudo apt-get install ghc cabal-install` and you're good to go.  (You
should also increase the swap space by editing `/etc/dphys-swapfile`.)
There are other ways to install more recent versions of GHC if you
want; see the [wiki page][5] for details.

This is a fairly early release, and there might be breaking changes in
the future.  Please contact me, by filing a GitHub issue, or by
[email][4], if you have any feedback on the library.  I'd also be
curious to hear how the library is being used!

[1]: https://www.adafruit.com/categories/808
[2]: https://learn.adafruit.com/adafruits-raspberry-pi-lesson-4-gpio-setup/configuring-i2c
[3]: https://www.cl.cam.ac.uk/~mgk25/ucs-fonts.html
[4]: http://funwithsoftware.org/contact.html
[5]: https://wiki.haskell.org/Raspberry_Pi
