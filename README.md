This package allows using an
[Adafruit character LCD and keypad kit][1] on a Raspberry Pi from
Haskell.

Everything you need to use one of these LCDs is exposed in the
`System.Hardware.PiLcd` module.  The submodules are only exposed
because they might be useful to reuse for other types of hardware,
such as if you are using an MCP23017 port expander or an HD44780
display controller in some other context.  But if you're using the
Adafruit character LCD and keypad kit, all you need is
`System.Hardware.PiLcd`.

Before using this package, you'll need to make sure that
[I2C is enabled][2] on your Raspberry Pi, under "Advanced Options" in
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

[1]: https://www.adafruit.com/categories/808
[2]: https://learn.adafruit.com/adafruits-raspberry-pi-lesson-4-gpio-setup/configuring-i2c
