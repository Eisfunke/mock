# Mock

![wAR miR jEtzT Zu anSTRENgEnd dA JedeN ZWEiten BUChSTaBeN GRoSS zU scHREiBen](mock.png)

There is a **Telegram bot** for Mock available as [@truemockbot](https://t.me/truemockbot), you can find its source code in the [repository](https://git.eisfunke.com/software/mock-telegram-bot).

## Installation

Just clone this repository or download a [release](https://git.eisfunke.com/software/mock/releases) and call

```
$ stack install
```

inside the folder. This will install Mock into `~/.local/bin`, which should be added to `$PATH`.

If you're using Arch Linux: there is also an [AUR package](https://aur.archlinux.org/packages/haskell-mock/) available.


## Usage

`mock [STYLE] [TEXT]`

You can get help a list of possible mock styles with `mock`.

If no text or "-" is given, input is read from `stdin`.

### Examples

```
$ mock random This is a great program.
ThIs Is A greaT PROgrAM.

$ mock space This is a great program.
T h i s   i s   a   g r e a t   p r o g r a m.
```

Mock calls can of course be concatenated with pipes:

```
$ mock space This is a great program. | mock random
T h I S   i S   A   G R e A t   P R o g r a m .
```


[![WTFPL-Badge](wtfpl.png)](http://www.wtfpl.net)
