CAN4DISCO firmware
==================

Written in http://ivorylang.org/

Requires
--------

- stack https://docs.haskellstack.org/en/stable/README/
- arm-none-eabi-newlib
- arm-none-eabi-gcc

Fedora::

  dnf install arm-none-eabi-gcc-cs arm-none-eabi-newlib


Building
--------

To build all images::

  ./standalone-setup.sh # required if you don't have checked out ivory tower and ivory-tower-stm32 repos in ..
  make

Tests
-----

CANSendRecv
  test application sending packets from CAN1, blinks on received packets.
  Image available in build/cansendrecv-test/image
CANLoop
  test application sending packets from CAN1 to CAN2 and vice versa
  triggering LEDs on received messages.
  Image available in build/canloop-test/image
CAN2UART
  can2uart bridge
  Image available in build/can2uart-test/image

Flashing
--------

With BlackMagic Probe::

  arm-none-eabi-gdb --ex 'target extended-remote /dev/ttyACM0' --ex 'monitor swdp_scan' --ex 'attach 1' --ex 'load' build/canloop-test/image
