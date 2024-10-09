use std::collections::HashMap;
use std::fs::File;
use std::io;
use std::io::Read;
use std::time::Instant;

use ratatui::{
    prelude::*,
    crossterm::event::{self, Event, KeyCode, KeyEventKind},
    widgets::*,
};

enum Void {}

const FONT: [[u8; 5]; 16] = [
    [0xF0, 0x90, 0x90, 0x90, 0xF0], // 0
    [0x20, 0x60, 0x20, 0x20, 0x70], // 1
    [0xF0, 0x10, 0xF0, 0x80, 0xF0], // 2
    [0xF0, 0x10, 0xF0, 0x10, 0xF0], // 3
    [0x90, 0x90, 0xF0, 0x10, 0x10], // 4
    [0xF0, 0x80, 0xF0, 0x10, 0xF0], // 5
    [0xF0, 0x80, 0xF0, 0x90, 0xF0], // 6
    [0xF0, 0x10, 0x20, 0x40, 0x40], // 7
    [0xF0, 0x90, 0xF0, 0x90, 0xF0], // 8
    [0xF0, 0x90, 0xF0, 0x10, 0xF0], // 9
    [0xF0, 0x90, 0xF0, 0x90, 0x90], // A
    [0xE0, 0x90, 0xE0, 0x90, 0xE0], // B
    [0xF0, 0x80, 0x80, 0x80, 0xF0], // C
    [0xE0, 0x90, 0x90, 0x90, 0xE0], // D
    [0xF0, 0x80, 0xF0, 0x80, 0xF0], // E
    [0xF0, 0x80, 0xF0, 0x80, 0x80], // F
];

enum Binop {
    Set, // VX := VY
    Or, // VX := VX | VY
    And, // VX := VX & VY
    Xor, // VX := VX ^ VY
    Add, // VX := VX + VY
    Sublr, // VX := VX - VY
    Subrl, // VX := VY - VX
    Shiftr, // Shift VX one bit to the right (ignore VY)
    Shiftl, // Shift VX one bit to the left (ignore VY)
}

// 'NN' means 8 bit number
// 'NNN' means 12 bit number

enum Instr {
    Cls, // Clear screen
    Ret, // Return from subroutine
    Jump(u16), // Jump to NNN
    Call(u16), // Call subroutine
    SkipImm(u8, u8, bool), // Skip next instruction if VX == NN (true)
                           // or VX != NN (false)
    Skip(u8, u8, bool), // Skip next instruction if VX == VY (true) or
                        // VX != VY (false)
    SetImm(u8, u8), // VX := NN
    AddImm(u8, u8), // VX += NN
    Op(Binop, u8, u8), // Binary operation
    SetIx(u16), // I := NNN
    JumpOffset(u16), // Jump to address NNN + V0
    Rand(u8, u8), // X := NN & random byte
    Display(u8, u8, u8), // Draw sprite at coordinates VX, VY with height N
    SkipIfKey(u8, bool), // Skip next instruction if key VX is pressed
                         // (true) or not pressed (false)
    GetDelayTimer(u8), // VX := delay timer
    SetDelayTimer(u8), // delay timer := VX
    SetSoundTimer(u8), // sound timer := VX
    AddToIx(u8), // I += VX
    GetKey(u8), // VX := pressed key (blocks until key is pressed)
    FontChar(u8), // I := address of font character in VX
    BinToDec(u8), // Store decimal digits of VX at [I:I+3]
    Store(u8), // Copy contents of V0-VX to memory starting at I
    Load(u8), // Copy contents of memory starting at I into V0-VX
}

struct Chip8 {
    memory: [u8; 4096],
    display: [[bool; 64]; 32],
    pc: u16,
    index_reg: u16,
    stack: Vec<u16>,
    delay_timer: u8,
    sound_timer: u8,
    regs: [u8; 16],
    scroll: usize,
    pad: [bool; 16],
}

const KEYS: [char; 16] = ['x', '1', '2', '3', 'q', 'w', 'e', 'a',
                          's', 'd', 'z', 'c', '4', 'r', 'f', 'v'];

fn key_index(key: char) -> Option<u8> {
    let padmap: HashMap<char, usize> =
        KEYS.into_iter().zip(0..KEYS.len()).collect();
    padmap.get(&key).map(|x| *x as u8)
}

impl Chip8 {
    fn new() -> Chip8 {
        Chip8 {
            memory: [0; 4096],
            display: [[false; 64]; 32],
            pc: 0x200,
            index_reg: 0, // I
            stack: Vec::new(),
            delay_timer: 0,
            sound_timer: 0,
            regs: [0; 16], // V0-VF
            scroll: 0,
            pad: [false; 16],
        }
    }

    fn fetch(&mut self) -> u16 {
        let bits: u16 =
            (self.memory[self.pc as usize] as u16) << 8
            | (self.memory[(self.pc + 1) as usize]) as u16;
        self.pc += 2;
        bits
    }

    fn decode(&self, bits: u16) -> Instr {
        use Instr::*;
        use Binop::*;
        let opcode = ((bits & 0xF000) >> 12) as u8;
        let x = ((bits & 0x0F00) >> 8) as u8;
        let y = ((bits & 0x00F0) >> 4) as u8;
        let n = (bits & 0x000F) as u8;
        let nn = (bits & 0x00FF) as u8;
        let nnn = bits & 0x0FFF;
        match opcode {
            0x0 => match nnn {
                0x0E0 => Cls,
                0x0EE => Ret,
                _ => panic!("unknown instruction: {:#x}", bits)
            }
            0x1 => Jump(nnn),
            0x2 => Call(nnn),
            0x3 => SkipImm(x, nn, true),
            0x4 => SkipImm(x, nn, false),
            0x5 => Skip(x, y, true),
            0x6 => SetImm(x, nn),
            0x7 => AddImm(x, nn),
            0x8 => Op(match n {
                0x0 => Set,
                0x1 => Or,
                0x2 => And,
                0x3 => Xor,
                0x4 => Add,
                0x5 => Sublr,
                0x6 => Shiftr,
                0x7 => Subrl,
                0xE => Shiftl,
                _ => panic!("unknown instruction: {:#x}", bits)
            }, x, y),
            0x9 => Skip(x, y, false),
            0xA => SetIx(nnn),
            0xB => JumpOffset(nnn),
            0xC => Rand(x, nn),
            0xD => Display(x, y, n),
            0xE => SkipIfKey(x, match nn {
                0x9E => true,
                0xA1 => false,
                _ => panic!("unknown instruction: {:#x}", bits)
            }),
            0xF => match nn {
                0x07 => GetDelayTimer(x),
                0x0A => GetKey(x),
                0x15 => SetDelayTimer(x),
                0x18 => SetSoundTimer(x),
                0x1E => AddToIx(x),
                0x29 => FontChar(x),
                0x33 => BinToDec(x),
                0x55 => Store(x),
                0x65 => Load(x),
                _ => panic!("unknown instruction: {:#x}", bits)
            }
            _ => panic!("unknown instruction: {:#x}", bits)
        }
    }

    fn execute(&mut self, instr: Instr) {
        use Instr::*;
        use Binop::*;
        match instr {
            Cls => {
                for row in self.display.iter_mut() {
                    for x in row {
                        *x = false
                    }
                }
            }
            Jump(nnn) => self.pc = nnn,
            Call(nnn) => {
                self.stack.push(self.pc);
                self.pc = nnn;
            }
            Ret => self.pc = self.stack.pop().unwrap(),
            SkipImm(x, nn, b) => {
                if (self.regs[x as usize] == nn) == b {
                    self.pc += 2;
                }
            }
            Skip(x, y, b) => {
                if (self.regs[x as usize] == self.regs[y as usize]) == b {
                    self.pc += 2;
                }
            }
            Op(binop, x, y) => {
                let vx = self.regs[x as usize];
                let vy = self.regs[y as usize];
                match binop {
                    Set => self.regs[x as usize] = vy,
                    Or => self.regs[x as usize] = vx | vy,
                    And => self.regs[x as usize] = vx & vy,
                    Xor => self.regs[x as usize] = vx ^ vy,
                    Add => {
                        let (v, overflow) = vx.overflowing_add(vy);
                        self.regs[x as usize] = v;
                        self.regs[15] = overflow as u8;
                    }
                    Sublr => {
                        if vx >= vy {
                            self.regs[x as usize] = vx - vy;
                            self.regs[15] = 1;
                        } else {
                            self.regs[x as usize] = 255 - (vy - vx) + 1;
                            self.regs[15] = 0;
                        }
                    }
                    Subrl => {
                        if vy >= vx {
                            self.regs[x as usize] = vy - vx;
                            self.regs[15] = 1;
                        } else {
                            self.regs[x as usize] = 255 - (vx - vy) + 1;
                            self.regs[15] = 0;
                        }
                    }
                    Shiftr => {
                        let temp = self.regs[x as usize];
                        self.regs[x as usize] = temp >> 1;
                        self.regs[15] = temp & 0b00000001;
                    }
                    Shiftl => {
                        let temp = self.regs[x as usize];
                        self.regs[x as usize] = temp << 1;
                        self.regs[15] = (temp & 0b10000000 > 0) as u8;
                    }
                }
            }
            SetIx(nnn) => {
                self.index_reg = nnn;
            }
            JumpOffset(nnn) => {
                self.pc = nnn + self.regs[0x0] as u16;
            }
            Rand(x, nn) => {
                self.regs[x as usize] = nn & rand::random::<u8>();
            }
            SkipIfKey(x, b) => {
                if self.pad[self.regs[(x & 0b00001111) as usize] as usize] == b {
                    self.pc += 2;
                }
            }
            GetDelayTimer(x) => {
                self.regs[x as usize] = self.delay_timer;
            }
            SetDelayTimer(x) => {
                self.delay_timer = self.regs[x as usize];
            }
            SetSoundTimer(x) => {
                self.sound_timer = self.regs[x as usize];
            }
            AddToIx(x) => {
                self.index_reg += self.regs[x as usize] as u16;
            }
            GetKey(x) => {
                loop {
                    if let Ok(Event::Key(key)) = event::read() {
                        if key.kind == KeyEventKind::Press {
                            if let KeyCode::Char(c) = key.code {
                                if let Some(i) = key_index(c) {
                                    self.regs[x as usize] = i;
                                    break
                                }
                            }
                        }
                    }
                }
            }
            FontChar(x) => {
                self.index_reg = self.regs[x as usize] as u16 * 5;
            }
            BinToDec(x) => {
                let num = self.regs[x as usize];
                let digit1 = num / 100;
                let digit2 = num % 100 / 10;
                let digit3 = num % 10;
                self.memory[self.index_reg as usize] = digit1;
                self.memory[self.index_reg as usize + 1] = digit2;
                self.memory[self.index_reg as usize + 2] = digit3;
            } // Store decimal digits of VX at [I:I+3]
            Store(x) => {
                for i in 0..(x+1) as usize {
                    self.memory[self.index_reg as usize + i] = self.regs[i];
                }
            } // copy contents of V0-VX to memory starting at I
            Load(x) => {
                for i in 0..(x+1) as usize {
                    self.regs[i] = self.memory[self.index_reg as usize + i];
                }
            } // Copy contents of memory starting at I into V0-VX
            SetImm(x, nn) => self.regs[x as usize] = nn,
            AddImm(x, nn) =>{
                let (v, _) = self.regs[x as usize].overflowing_add(nn);
                self.regs[x as usize] = v;
            }
            Display(x, y, n) => {
                let x_coord = self.regs[x as usize] % 64;
                let y_coord = self.regs[y as usize] % 32;
                self.regs[0xF as usize] = 0;
                for i in 0..n {
                    if y_coord + i >= 32 {
                        break
                    };
                    let byte = self.memory[(self.index_reg + i as u16) as usize];
                    for j in 0..8 {
                        if x_coord + j >= 64 {
                            break
                        };
                        let bit = byte & 1 << (7 - j);
                        let old_pixel =
                            self.display[(y_coord + i) as usize][(x_coord + j) as usize];
                        let new_pixel = if bit != 0 { !old_pixel } else { old_pixel };
                        self.display[(y_coord + i) as usize][(x_coord + j) as usize] =
                            new_pixel;
                        if !old_pixel && new_pixel {
                            self.regs[0xF] = 1;
                        }
                    }
                }
            }
        }
    }

    fn process_events(&mut self) -> Result<(), String> {
        while let Ok(true) = event::poll(std::time::Duration::from_secs(0)) {
            match event::read().unwrap() {
                Event::Key(key) => {
                    if key.kind == KeyEventKind::Press {
                        match key.code {
                            KeyCode::Esc => return Err("exit".into()),
                            KeyCode::PageUp =>
                                self.scroll =
                                if self.scroll > 0 { self.scroll - 1 } else { 0 },
                            KeyCode::PageDown => self.scroll += 1,
                            KeyCode::Char(c) => {
                                if let Some(i) = key_index(c) {
                                    self.pad[i as usize] ^= true;
                                }
                            }
                            _ => ()
                        }
                    }
                }
                _ => ()
            }
        }
        Ok(())
    }

    fn render_reg(frame: &mut Frame, area: Rect, name: &str, val: u16, len: u16) {
        let [left, right] = Layout::horizontal([
            Constraint::Length(std::cmp::max(2, name.len() as u16)),
            Constraint::Length(len)])
            .areas(area);
        frame.render_widget(Paragraph::new(name)
                            .white()
                            .on_dark_gray(),
                            left);
        frame.render_widget(Paragraph::new(format!("{}", val))
                            .white()
                            .on_black()
                            .right_aligned(),
                            right);
    }

    fn render_pad(&self, frame: &mut Frame, area: Rect) {
        let mk_cell = |i| {
            let keys = ['1', '2', '3', 'C', '4', '5', '6', 'D',
                        '7', '8', '9', 'E', 'A', '0', 'B', 'F'];
            let ixs = [1, 2, 3, 12, 4, 5, 6, 13, 7, 8, 9, 14, 10, 0, 11, 15];
            Cell::from(format!("{}", keys[i]))
                .style(if self.pad[ixs[i]] { Style::new().red() }
                       else { Style::new() })
        };
        let rows = [Row::new((0..4).map(|x| mk_cell(x))).height(1),
                    Row::new((4..8).map(|x| mk_cell(x))).height(1),
                    Row::new((8..12).map(|x| mk_cell(x))).height(1),
                    Row::new((12..16).map(|x| mk_cell(x))).height(1)];
        let widths = [
            Constraint::Length(1),
            Constraint::Length(1),
            Constraint::Length(1),
            Constraint::Length(1),
        ];
        frame.render_widget(Table::new(rows, widths)
                            .block(Block::new()
                                   .title("pad")
                                   .title_style(Style::new().black())
                            ), area.inner(Margin {
                                vertical: 1,
                                horizontal: 0,
                            }));
    }

    fn render(&self, terminal: &mut Terminal<CrosstermBackend<io::Stdout>>) {
        let _ = terminal.draw(|frame| {

            let [left, right] =
                Layout::horizontal([Constraint::Length(64),
                                    Constraint::Fill(1)]).spacing(1)
                .areas(frame.area());

            let [top_left, bottom_left] =
                Layout::vertical([Constraint::Length(32),
                                  Constraint::Fill(1)]).spacing(1)
                .areas(left);

            let [bottom_left_left, bottom_left_right] =
                Layout::horizontal([Constraint::Percentage(50),
                                    Constraint::Fill(1)]).spacing(1)
                .areas(bottom_left);


            let mut display_s = String::with_capacity(32*64);
            for row in self.display {
                for x in row {
                    display_s += if x { "█" } else { " " };
                }
                display_s += "\n";
            }

            frame.render_widget(Paragraph::new(display_s).white().on_black(), top_left);

            let mut mem_s = String::new();
            for byte in self.memory.iter() {
                mem_s += &format!("{:X}", byte);
            }
            frame.render_widget(Paragraph::new(mem_s.clone())
                                .white()
                                .on_black()
                                .block(Block::bordered()
                                       .title("memory")
                                       .border_style(Style::new().light_blue())
                                       .border_type(BorderType::Rounded)
                                )
                                .wrap(Wrap { trim: true })
                                .scroll((self.scroll as u16, 0)),
                                right);

            let scrollbar = Scrollbar::new(ScrollbarOrientation::VerticalRight)
                .begin_symbol(Some("↑"))
                .end_symbol(Some("↓"));
            let mut scrollbar_state =
                ScrollbarState::new(mem_s.len() / right.width as usize)
                .position(self.scroll);
            frame.render_stateful_widget(
                scrollbar,
                right.inner(Margin {
                    vertical: 1,
                    horizontal: 0,
                }),
                &mut scrollbar_state,
            );

            frame.render_widget(Paragraph::new("")
                                .white()
                                .on_blue()
                                .wrap(Wrap { trim: true })
                                .block(Block::bordered()
                                       .title("registers")
                                       .border_style(Style::new().black())
                                       .border_type(BorderType::QuadrantOutside)),
                                bottom_left_left);

            let [reg_pc_delay, reg_i_sound,
                 reg_0_4, reg_4_8, reg_8_12, reg_12_16, pad] =
                Layout::vertical([Constraint::Length(1),
                                  Constraint::Length(1),
                                  Constraint::Length(1),
                                  Constraint::Length(1),
                                  Constraint::Length(1),
                                  Constraint::Length(1),
                                  Constraint::Fill(1)])
                .areas(bottom_left_left.inner(Margin {
                    vertical: 1,
                    horizontal: 1,
                }));

            let [reg_pc, reg_delay] = Layout::horizontal([Constraint::Length(9),
                                                          Constraint::Fill(1)])
                .areas(reg_pc_delay);
            Self::render_reg(frame, reg_pc, "pc", self.pc, 6);
            Self::render_reg(frame, reg_delay, "delay", self.delay_timer.into(), 6);

            let [reg_i, reg_sound] = Layout::horizontal([Constraint::Length(9),
                                                         Constraint::Fill(1)])
                .areas(reg_i_sound);
            Self::render_reg(frame, reg_i, "I", self.index_reg, 6);
            Self::render_reg(frame, reg_sound, "sound", self.sound_timer.into(), 6);

            let vreg_areas = [reg_0_4, reg_4_8, reg_8_12, reg_12_16];
            for i in 0..4 {
                let rects =
                    Layout::horizontal([Constraint::Length(6),
                                        Constraint::Length(6),
                                        Constraint::Length(6),
                                        Constraint::Length(6)]).spacing(1)
                    .areas::<4>(vreg_areas[i]);
                for (j, a) in rects.into_iter().enumerate() {
                    Self::render_reg(frame, a,
                                     &format!("V{:X}", i + 4*j),
                                     self.regs[i + 4*j] as u16, 4);
                }
            }

            frame.render_widget(Paragraph::new("")
                                .white()
                                .on_blue()
                                .wrap(Wrap { trim: true })
                                .block(Block::new()
                                       .title_style(Style::new().black())
                                       .title("pad")),
                                pad.inner(Margin {
                                    vertical: 1,
                                    horizontal: 0,
                                }));

            self.render_pad(frame, pad);

            let mut stack_s = String::new();
            for x in self.stack.iter() {
                stack_s += &format!("{:X}\n", x);
            }
            frame.render_widget(Paragraph::new(stack_s)
                                .white()
                                .on_blue()
                                .block(Block::bordered()
                                       .title("call stack")
                                       .border_style(Style::new().black())
                                       .border_type(BorderType::QuadrantOutside)),
                                bottom_left_right);

        });
    }

    fn run(&mut self, mut terminal: Terminal<CrosstermBackend<io::Stdout>>)
           -> Result<Void, String> {
        let mut time = Instant::now();
        loop {
            self.process_events()?;

            let new_time = Instant::now();
            if new_time.duration_since(time).as_secs_f64() >= 1.0 / 60.0 {
                self.delay_timer =
                    if self.delay_timer > 0 { self.delay_timer - 1} else { 0 };
                self.sound_timer =
                    if self.sound_timer > 0 { self.sound_timer - 1} else { 0 };
                time = new_time;
            }

            if self.sound_timer > 0 {
                println!("\x07");
            }

            let instr_bytes = self.fetch();
            let instr = self.decode(instr_bytes);
            self.execute(instr);

            self.render(&mut terminal)
        }
    }
}

fn main() -> io::Result<()> {
    let mut chip8 = Chip8::new();
    for (i, bytes) in FONT.iter().enumerate() {
        chip8.memory[i*5..(i+1)*5].copy_from_slice(bytes);
    }

    let prog = "roms/Sierpinski [Sergey Naydenov, 2010].ch8";
    let mut f = File::open(prog)?;
    f.read(&mut chip8.memory[0x200..])?;

    let terminal = ratatui::init();
    let _ = chip8.run(terminal);

    ratatui::restore();
    Ok(())
}
