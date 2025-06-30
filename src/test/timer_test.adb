procedure Timer_Test is
Prescaler : constant UInt16 := UInt16 (((System_Clock_Frequencies.SYSCLK / 2) / 6000) - 1);
Channel_1_Period : constant := 6000 - 1;                          -- 1 sec
begin
      -- Virtual ADC
      Enable_Clock (Timer_2);

      Configure
         (Timer_2,
            Prescaler     => Prescaler,
            Period        => UInt32 (UInt16'Last),  -- all the way up
            Clock_Divisor => Div1,
            Counter_Mode  => Up);

      Configure_Prescaler
         (Timer_2,
         Prescaler   => Prescaler,
         Reload_Mode => Immediate);

      Enable_Interrupt
         (Timer_2, STM32.Timers.Timer_CC1_Interrupt);

       Configure_Channel_Output
        (Timer_2,
         Channel  => Channel_1,
         Mode     => Frozen,
         State    => Enable,
         Pulse    => Channel_1_Period,
         Polarity => High);

      Set_Output_Preload_Enable (Timer_2, Channel_1, False);

      Enable (Timer_2);

end Timer_Test;