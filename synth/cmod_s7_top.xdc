# This file is a general .xdc for the Cmod S7-25 Rev. B
# Based on https://github.com/Digilent/digilent-xdc/blob/master/Cmod-S7-25-Master.xdc

# 12 MHz System Clock
set_property BOARD_PIN clk                           [get_ports { sys_clk_i }  ];

# Push Buttons
set_property BOARD_PIN push_buttons_2bits_tri_i_0    [get_ports { btn_i[0] }   ];
set_property BOARD_PIN push_buttons_2bits_tri_i_1    [get_ports { btn_i[1] }   ];

# RGB LEDs
set_property BOARD_PIN rgb_led_3bits_tri_io_0        [get_ports { led0_b_o }   ];
set_property BOARD_PIN rgb_led_3bits_tri_io_1        [get_ports { led0_g_o }   ];
set_property BOARD_PIN rgb_led_3bits_tri_io_2        [get_ports { led0_r_o }   ];

# 4 LEDs
set_property BOARD_PIN led_4bits_tri_io_0            [get_ports { led_o[0] }   ];
set_property BOARD_PIN led_4bits_tri_io_1            [get_ports { led_o[1] }   ];
set_property BOARD_PIN led_4bits_tri_io_2            [get_ports { led_o[2] }   ];
set_property BOARD_PIN led_4bits_tri_io_3            [get_ports { led_o[3] }   ];

# USB UART
set_property BOARD_PIN usb_uart_txd                  [get_ports { uart_tx_o }  ];
set_property BOARD_PIN usb_uart_rxd                  [get_ports { uart_rx_i }  ];


########################
# Timing constraints
########################

create_clock -add -name sys_clk -period 83.33        [get_ports { sys_clk_i }]; # 12 MHz

set_property IOB TRUE                                [get_ports { uart_rx_i }  ];
set_property IOB TRUE                                [get_ports { uart_tx_o }  ];


########################
# Board constraints
########################

set_property BITSTREAM.CONFIG.CONFIGRATE 33    [current_design]
set_property BITSTREAM.GENERAL.COMPRESS  TRUE  [current_design]
set_property CFGBVS                      VCCO  [current_design]
set_property CONFIG_MODE                 SPIx4 [current_design]
set_property CONFIG_VOLTAGE              3.3   [current_design]

