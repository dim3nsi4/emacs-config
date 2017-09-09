# emacs-config
Yet Another GNU Emacs Configuration.

The main configuration file is `init.el` file, which
should be placed under `.emacs.d/` in your home directory. It relies
on [use-package](https://github.com/jwiegley/use-package)
and [req-package](https://github.com/edvorg/req-package) to manage the
loading of the packages and their configuration.

Several packages are activated if they are detected
(e.g. [ivy](https://github.com/abo-abo/swiper),
[projectile](https://github.com/bbatsov/projectile),
[company](https://github.com/company-mode/company-mode), ...). The
configuration of these packages is done in the files located under
`.emacs.d/config/modes`.

This configuration also uses a custom theme named `clearview-light`,
which must also be placed under `.emacs.d/` in your home
directory. This theme can easily be customized using
`M-x custom-visit-theme`.

For more details, see the [wiki](../../wiki) (under construction at
the moment, may be outdated).

![alt tag](screenshot-emacs.png)
