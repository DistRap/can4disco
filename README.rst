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
SLCAN
  SLCAN loopback test. Connect CAN1 to CAN2 create loopback
  for SLCAN. SLCAN messages sent to UART2 will flow thru CAN1->CAN2 back to UART2,
  see SLCAN usage for more information.
  Image available in build/slcan-test/image

Flashing
--------

With BlackMagic Probe::

  arm-none-eabi-gdb --ex 'target extended-remote /dev/ttyACM0' --ex 'monitor swdp_scan' --ex 'attach 1' --ex 'load' build/canloop-test/image

SLCAN usage
-----------

SLCAN allows transmission of decapsulated CAN frames over UART. To create
UART<->CAN bridge with Linux kernel use::

  modprobe can
  modprobe can-raw
  modprobe slcan
  slcand -F -s8 -S115200 /dev/f4uart can0 # CAN speed 8 -> 1Mbit
  ip link set can0 up
  candump can0
  cansend can0 1F334455#112233445566778
  cansend can0 1EF#11223344556677FF

Debugging SLCAN
~~~~~~~~~~~~~~~

To send raw messages to can4disco::

  echo -n -e 'TFF3344558DEADBEEFAABBCCDD\r' > /dev/f4uart
  echo -n -e 't1234DEADBEEF\r' > /dev/f4uart

To fake UART for `slcand` use::

  socat -d -d pty,link=/dev/ttyS0,raw,echo=0 pty,link=/dev/ttyS1,raw,echo=0

run screen on one side::

  screen /dev/ttyS1 115200

and point slcand to it::

  slcand -F -s8 -S115200 /dev/ttyS0 can0
