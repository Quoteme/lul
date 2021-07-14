# Lul - Window manager

## Screenshots

![https://i.imgur.com/pxSuvx7.png](https://i.imgur.com/pxSuvx7.png)

## Development requirements

- xorg-server-xephyr

```
Xephyr :1 -ac -br -screen 1024x768 -resizeable -reset -terminate
export DISPLAY=:1
```

```
cabal run
export DISPLAY=:0
```
