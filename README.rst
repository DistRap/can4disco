CAN4DISCO firmware
==================

Written in http://ivorylang.org/

Tests
-----

CANSendRecv
  test application sending packets from CAN1, blinks on received packets.
CANLoop
  test application sending packets from CAN1 to CAN2 and vice versa
  triggering LEDs on received messages.
CAN2UART
  can2uart bridge
SLCANLoopback
  SLCAN loopback test. Connect CAN1 to CAN2 create loopback
  for SLCAN. SLCAN messages sent to UART2 will flow thru CAN1->CAN2 back to UART2,
  see SLCAN usage for more information.
SLCAN
  Standalone UART2 <-> CAN1 bridge

SLCAN usage
-----------

SLCAN allows transmission of decapsulated CAN frames over UART. To create
UART<->CAN bridge with Linux kernel use::

  modprobe can
  modprobe can-raw
  modprobe slcan
  slcand -F -s8 -S115200 /dev/can4disco-uart can0 # CAN speed 8 -> 1Mbit
  ip link set can0 up
  candump can0
  cansend can0 1F334455#112233445566778
  cansend can0 1EF#11223344556677FF

You can also use::

  make slcan-setup

to execute `./util/setup` script (runs commands above, requires sudo).

Debugging SLCAN
~~~~~~~~~~~~~~~

To send raw messages to can4disco::

  echo -n -e 'TFF3344558DEADBEEFAABBCCDD\r' > /dev/can4disco-uart
  echo -n -e 't1234DEADBEEF\r' > /dev/can4disco-uart

To fake UART for `slcand` use::

  socat -d -d pty,link=/dev/ttyS0,raw,echo=0 pty,link=/dev/ttyS1,raw,echo=0

run screen on one side::

  screen /dev/ttyS1 115200

and point slcand to it::

  slcand -F -s8 -S115200 /dev/ttyS0 can0

Running SLCAN POSIX Loopback
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is possible to run `slCANTower` on POSIX with the help of `ivory-tower-posix`.

Build the executable with::

  make slcan-posix-loopback-test

Link `ttyS0` and `ttyS1`::

  socat -d -d pty,link=/dev/ttyS0,raw,echo=0 pty,link=/dev/ttyS1,raw,echo=0

run `tower_init` on one side::

  ./build/slcan-posix-loopback-test/tower_init <> /dev/ttyS1 > /dev/ttyS1

and `slcand` on the other side::

  slcand -F -s8 -S115200 /dev/ttyS0 can0
  ip link set can0 up

Try dumping and sending some data, your messages should appear twice as
there is an loopback in `tower_init`::

  candump can0
  cansend can0 000#0201

Testing hardware
----------------

Load loopback firmware::

  make canloop-test

Connect `CAN1 L` to `CAN2 L` and `CAN1 H` to `CAN2 H` to create a hardware loopback, after bridging
both transcievers `RX/TX` `LEDs` should start blinking indicating reception on both interfaces.
