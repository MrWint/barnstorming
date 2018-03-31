; Annotated disassembly of Barnstorming (NTSC) for Atari 2600

; Assemble with : dasm barnstorming.asm -f3 -obarnstorming.bin


; TIA read registers
CXP0FB  =  $02
CXPPMM  =  $07
INPT4   =  $0C
; TIA write registers
VSYNC   =  $00
VBLANK  =  $01
WSYNC   =  $02
NUSIZ0  =  $04
NUSIZ1  =  $05
COLUP0  =  $06
COLUP1  =  $07
COLUPF  =  $08
COLUBK  =  $09
CTRLPF  =  $0A
REFP0   =  $0B
REFP1   =  $0C
PF0     =  $0D
PF1     =  $0E
PF2     =  $0F
RESP0   =  $10
RESP1   =  $11
AUDC0   =  $15
GRP0    =  $1B
GRP1    =  $1C
ENAM0   =  $1D
ENAM1   =  $1E
HMP0    =  $20
HMP1    =  $21
HMOVE   =  $2A
HMCLR   =  $2B
CXCLR   =  $2C

; PIA read registers
SWCHA   =  $0280
SWCHB   =  $0282
INTIM   =  $0284
; PIA write registers
TIM64T  =  $0296

; RAM addresses
GameMode                      = $80 ; 0-3
GlobalFrameCounter            = $81
RNG_82                        = $82 ; bit shifting PRNG, only incremented on mode 4 select, used to randomize initial object index for game mode 4
GameModeSwitchCooldown        = $83 ; counts down from $1e when holding down select
JoystickInputs                = $84 ; 0000rldu, 1 = not pressed, 0 = pressed
ColorsModifier1               = $85 ; used in inactive state to blink screen colors
ColorsModifier2               = $86
Colors                        = $87 ; $87-$8e, array of length 8
PlayerYPosition               = $8f ; $14 (top) - $7d (ground)
PropellerGraphicsCycle        = $90 ; 0-2, increases every 4 frames w/o, every 2 frames w/ pressing B
WindmillGraphicsCycle         = $91 ; 0-2, increases every 4 frames
BarnsLeftDigitPointers        = $92 ; $92-$95, $92 = 10s digit, $94 = 1s digit
GameModeDigitPointer          = $96 ; $96-$97, points to graphics of digit displaying the current game mode
PTR_98                        = $98 ; $98-$99 used as pointer storage in multiple places
PropellerGraphicsPointer      = $9a ; $9a-$9b
BirdGraphicsAnimationPointer  = $9c ; $9c-$9d, pointer to current animation frame graphics
; $9e-a1 unused
BarnGraphicsUpperHalf         = $a2 ; roof graphics bitfield, 8px chunks, 1=render, usually $ff, used to hide offscreen parts of the barn
BarnGraphicsLowerHalf         = $a3 ; wall graphics bitfield, 8px chunks, 1=render, usually $fe, used to hide offscreen parts of the barn, 4px offset to roof
CurrentObjectIndex            = $a4 ; index into object data, $00-$3f
NextBirdLaneCycle             = $a5 ; values $00-$0f, determines which lane birds will spawn next
WindmillGraphicsPointer       = $a6 ; $a6-$a7
InactivityTimer               = $a8 ; ticks up every 256 frames, mutes game at $20+, pauses game and flashes colors at $80+
EngineSoundVolume             = $a9 ; 0-3, starts at 0, increases after start and never decreases
CollisionDeflectionDirection  = $aa ; 0 = up and forward, 1 = down and forward, 2 = up and backward, $ff = no ongoing collision
CollisionCooldownTimerDoubled = $ab ; counts down from $46 upon collision
CollisionCooldownTimer        = $ac ; counts down from $23 upon collision
BirdLaneCollisionTimer        = $ad ; $ad-af counts down from $30 when colliding with bird on lane
GameWonFlag                   = $b0 ; $00 when running, $ff when won
ObjectCollisionFlag           = $b1 ; bit7 = collided with object
BirdLaneCollisionFlag         = $b2 ; $b2-$b4, bit7 = collided with bird lane
InGameTime                    = $b5 ; $b5-$b8, BCD timer in minutes, seconds, and hundreds, and a binary 1/25600th second counter
InGameTimeDisplayPointers     = $b9 ; $b9-$c2
BirdsPassedCounter            = $c3 ; $c3-$c5, number of bird formations passed on each lane (%4)
BirdFormation                 = $c6 ; $b6-$b8, type of bird formation in a lane. 0 = single, 1 = 10 spaced, 2 = 20 spaced, 4 = 40 spaced
AudioControlRegisters         = $c9 ; $c9-$ce caches audio control/freq/volume registers, written once per frame
GameStartedFlag               = $cf ; started by pressing Button
ActiveBirdLane                = $d0 ; bit0-bit2 = top.middle,bottom lane, 1 = birds on lane
PlayerDisabled                = $d1 ; game has ended
BarnScoreCooldownTimer        = $d2 ; counts down from $50 after scoring a barn to avoid double-scoring
NumberOfBarnsLeft             = $d3 ; BCD number
EngineSoundPitch              = $d4 ; 0-7, higher = higher pitch
PlayerVerticalSpeed           = $d5 ; 0 - $17
PlayerSpeed                   = $d6
PlayerDisplacement            = $d7 ; $d7-$d8, $d7 = vertical, $d8 = horizontal
PlayerDisplacementFractional  = $d9 ; $d9-$da, $d9 = vertical, $da = horizontal
TMP_BirdLane_DB               = $db
BirdGraphicsAnimationCycle    = $dc ; 0-7, increases every 8 frames
FencePostGraphicsPosition     = $dd ; horizonal position of fence posts, $9f-$00
Unused_Always0_DE             = $de
BarnEndPosition               = $df ; descreases from $b7 to $00, by $18 larger than CurrentObjectPosition
; $e0 unused value affected by BirdGraphics_NumberSize loops
BirdGraphics_NumberSize       = $e1 ; $e1-$e3; Used in NUSIZ0 register to render birds
; $e4 unused value affected by BirdGraphics_Position loops
BirdGraphics_Position         = $e5 ; $e5-$e7
CurrentObjectPosition         = $e8 ; descreases from $9f to $00
BirdPosition                  = $e9 ; $e9-$eb: {bottom, middle, top} lane
BirdGraphics_HMP0             = $ec ; value written to HMP0 register
; $ed unused
BirdGraphicsLaneAnimationLowAddr = $ee ; $ee-$f0: lower byte of current animation frame graphics by lane, uses either $f1 or $f2
BirdGraphicsAnimationLowAddr  = $f1 ; lower byte of current animation frame graphics ($ffXX)
BirdGraphicsStartledAnimationLowAddr = $f2 ; lower byte of current animation frame graphics ($ffXX), used if bird were hit
CurrentObjectIsBarnFlag       = $f3 ; 1 if current object is barn, 0 if windmill or empty
CurrentObjectRenderFlag       = $f4 ; render windmill/weather vane (barn itself is independent from this); affects collisions
TMP_F5                        = $f5 ; temporary variable used in multiple places
TMP_F6                        = $f6 ; temporary variable used in multiple places
TMP_F7                        = $f7 ; temporary variable used in multiple places
TMP_F8                        = $f8 ; temporary variable used in multiple places
Unused_F9                     = $f9 ; only ever written to, never read
; fa-ff used for stack


       processor 6502
       ORG $F000

START:
       SEI
       CLD
       LDX    #$00
StartupClearMem: LDA    #$00
ClearMemoryLoop:
       STA    $00,X
       TXS
       INX
       BNE    ClearMemoryLoop
       JSR    SetupInitialGameState
       LDA    RNG_82
       BNE    MainGameLoop
       LDX    #$01
       STX    RNG_82
       DEX
       JMP    ColdBoot
MainGameLoop:
       LDX    #$07
LF01D: LDA    ColorsData,X
       EOR    ColorsModifier1
       AND    ColorsModifier2
       STA    Colors,X
       CPX    #$01
       BCS    LF02C
       STA    COLUBK,X
LF02C: DEX
       BPL    LF01D
       LDA    #$21
       LDX    #$00
       JSR    LFDBD
       LDA    #$28
       INX
       STX    CTRLPF
       JSR    LFDBD
       LDA    FencePostGraphicsPosition
       INX
       JSR    LFDBD
       LDA    FencePostGraphicsPosition
       CLC
       ADC    #$40
       CMP    #$A0
       BCC    LF04F
       SBC    #$A0
LF04F: INX
       JSR    LFD96
       STA    WSYNC
       STA    HMCLR
       LDX    #$03
LF059: LDA    BirdPosition-1,X
       JSR    LFD9E
       STA    TMP_F5
       LDA    BirdGraphics_NumberSize-1,X
       AND    #$0F
       ORA    TMP_F5
       STA    BirdGraphics_NumberSize-1,X
       STA    BirdGraphics_NumberSize-1,X
       DEY
       DEY
       DEY
       DEY
       STY    BirdGraphics_Position-1,X
       DEX
       BPL    LF059
       LDA    Colors+4
       STA    COLUBK
LF077: LDY    INTIM
       BNE    LF077
       STA    WSYNC
       STA    HMOVE
       STY    VBLANK
       STY    NUSIZ0
       STY    NUSIZ1
       LDA    #$02
       EOR    ColorsModifier1
       AND    ColorsModifier2
       STA    COLUP0
       STA    COLUP1
       LDX    #$01
LF092: STA    WSYNC
       STA    HMOVE
       DEX
       BPL    LF092
       LDY    #$07
LF09B: STA    WSYNC
       STA    HMOVE
       LDA    (BarnsLeftDigitPointers+0),Y ; 10s digit
       STA    GRP0
       LDA    (BarnsLeftDigitPointers+2),Y ; 1s digit
       STA    GRP1
       DEY
       BPL    LF09B
       JMP    LF0F1
ColorsData: .byte $D6,$00,$D0,$1A,$88,$0E,$14,$12
BarnColorsData: .byte $42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42
       .byte $42,$42,$42,$42,$42,$42,$42,$42,$D0,$06,$06,$06,$06,$06,$06,$06
       .byte $04,$04,$04,$04,$04,$04,$04,$02,$02,$02,$02,$02,$02,$02,$D0,$D6
LF0E5: .byte $FF,$FF,$EE,$88,$00,$00,$00,$00
NumberOfBarnsData: .byte $10,$15,$15,$25
LF0F1: STA    WSYNC
       STA    HMOVE
       INY
       STY    GRP0
       STY    GRP1
       INY
       STY    NUSIZ1
       LDY    #$03
       STY    NUSIZ0
       NOP
       STA    Unused_F9
       STA    RESP0
       STA    RESP1
       LDX    #$01
LF10A: STA    WSYNC
       STA    HMOVE
       LDY    #$00
       STA    Unused_F9
       DEC    TMP_F5
       LDA    #$07
       STA    TMP_F5
       LDA    InGameTimeDisplayPointers+2 ; lower byte of ten seconds digit graphics pointer, used to show/hide decimal points
       CMP    #$50
       DEX
       STA    HMCLR
       BPL    LF10A
       BCC    LF128
       STY    NUSIZ0
       STY    NUSIZ1
       DEX
LF128: STX    TMP_F7
LF12A: LDY    TMP_F5
       LDA    (InGameTimeDisplayPointers),Y
       ORA    LFDD7,Y
       AND    TMP_F7
       STA    GRP0
       LDA    (InGameTimeDisplayPointers+2),Y
       STA    GRP1
       STA    WSYNC
       STA    HMOVE
       LDA    (InGameTimeDisplayPointers+8),Y
       STA    TMP_F6
       LDA    (InGameTimeDisplayPointers+6),Y
       TAX
       LDA    (InGameTimeDisplayPointers+4),Y
       ORA    LFDDC,Y
       LDY    TMP_F6
       STA    GRP0
       STX    GRP1
       STY    GRP0
       DEC    TMP_F5
       BPL    LF12A
       STA    WSYNC
       STA    HMOVE
       LDA    #$00
       STA    GRP0
       STA    GRP1
       JMP    LF170
LF162: .byte $14,$14,$0E,$14,$14,$0E,$14,$14,$14,$14,$14,$14,$14,$14
LF170: LDX    #$0C
LF172: LDA    LFF93,X
       EOR    ColorsModifier1
       AND    ColorsModifier2
       CPX    #$08
       BCS    LF199
       STA    WSYNC
       STA    HMOVE
       STA    COLUBK
       LDA    Colors+7
       STA    COLUPF
       LDA    LF0E5,X
       STA    PF0
       LDA    LFDE4,X
       STA    PF1
       LDA    LFDEC,X
       STA    PF2
       JMP    LF19F
LF199: STA    WSYNC
       STA    HMOVE
       STA    COLUBK
LF19F: DEX
       BPL    LF172
       LDX    #$00
       LDY    Colors+0
       LDA    Colors+3
       STA    WSYNC
       STA    HMOVE
       STA    COLUPF
       STY    COLUBK
       STX    PF0
       STX    PF1
       STX    PF2
       STX    NUSIZ1
       STX    CTRLPF
       LDA    Colors+2
       STA    RESP1
       STA    COLUP1
       LDA    #$F0
       STA    HMP1
       STA    WSYNC
       STA    HMOVE
       JSR    LFDBC
       JSR    LFDBC
       STA    HMCLR
       LDX    #$04
       STX    TMP_BirdLane_DB
       LDY    PlayerYPosition
LF1D6: STA    WSYNC
       STA    HMOVE
       CPY    #$15
       BCS    LF207
       LDA    (PropellerGraphicsPointer),Y
       STA    GRP1
       LDA    LFF45,Y
       STA    PF1
LF1E7: DEX
       STX    TMP_BirdLane_DB
       LDA    BirdGraphics_NumberSize-1,X
       STA    NUSIZ0
       STA    BirdGraphics_HMP0
       LDA    #$00
       STA    PF1
       DEY
       CPY    #$15
       BCS    LF212
       LDA    (PropellerGraphicsPointer),Y
       STA    GRP1
       LDA    LFF45,Y
LF200: STY    TMP_F5
       STA    PF1
       JMP    LF21B
LF207: LDA    #$00
       STA    PF1
       STA    GRP1
       STA    Unused_F9
       JMP    LF1E7
LF212: LDA    #$00
       STA    PF1
       STA    GRP1
       JMP    LF200
LF21B: STA    WSYNC
       STA    HMOVE
       LDA    BirdGraphics_Position-1,X
       TAX
       BMI    LF245
       CPX    #$05
       BCS    LF271
       CPX    #$02
       BCS    LF255
LF22C: DEX
       BPL    LF22C
       NOP
       STA    RESP0
LF232: JSR    LFDBC
       LDA    #$00
       STA    PF1
       DEY
       CPY    #$15
       BCS    LF242
       LDA    (PropellerGraphicsPointer),Y
       STA    GRP1
LF242: JMP    LF291
LF245: STA    Unused_F9
       STA    Unused_F9
       STA    Unused_F9
       .byte $8D,$10,$00 ; STA $10
       LDA    #$60
       STA    HMP0
       JMP    LF232
LF255: STA    Unused_F9
       DEX
       DEX
       STA    Unused_F9
LF25B: DEX
       BPL    LF25B
       .byte $8D,$10,$00 ; STA $10
       LDA    #$00
       STA    PF1
       DEY
       CPY    #$15
       BCS    LF26E
       LDA    (PropellerGraphicsPointer),Y
       STA    GRP1
LF26E: JMP    LF291
LF271: SBC    #$05
       TAX
       DEY
       CPY    #$15
       BCC    LF280
       STA    Unused_F9
       DEC    TMP_F5
       JMP    LF285
LF280: LDA    (PropellerGraphicsPointer),Y
       NOP
       STA    GRP1
LF285: LDA    #$00
       STA    PF1
       STA    Unused_F9
LF28B: DEX
       BPL    LF28B
       .byte $8D,$10,$00 ; STA $10
LF291: STA    WSYNC
       STA    HMOVE
       CPY    #$15
       BCC    LF29F
       NOP
       STA    Unused_F9
       JMP    LF2A4
LF29F: LDA    LFF45,Y
       STA    PF1
LF2A4: LDX    TMP_BirdLane_DB
       BEQ    LF308
       .byte $AD,$F1,$00 ; LDA BirdGraphicsAnimationLowAddr
       STA    BirdGraphicsAnimationPointer
       LDA    BirdGraphics_HMP0
       STA    HMP0
       LDA    Colors+5
       .byte $8D,$06,$00 ; STA $06
       LDX    #$0C
LF2B8: LDA    Unused_Always0_DE
       STA    PF1
       DEY
       CPY    #$15
       BCS    LF2FB
       LDA    (PropellerGraphicsPointer),Y
       STA    GRP1
       LDA    LFF45,Y
       STA    PF1
       STY    TMP_F5
       TXA
       TAY
LF2CE: STA    HMOVE
       LDX    TMP_BirdLane_DB
       LDA    BirdGraphicsLaneAnimationLowAddr-1,X
       STA    BirdGraphicsAnimationPointer
       LDA    (BirdGraphicsAnimationPointer),Y
       STA    GRP0
       TYA
       TAX
       LDY    TMP_F5
       LDA    CXPPMM
       STA    Unused_F9
       STA    HMCLR
       DEX
       BPL    LF2B8
       LDX    #$00
       STX    BirdGraphics_HMP0
       STX    PF1
       LDX    TMP_BirdLane_DB
       DEY
       ORA    CXP0FB
       ORA    BirdLaneCollisionFlag-1,X
       STA    BirdLaneCollisionFlag-1,X
       STA    CXCLR
       JMP    LF1D6
LF2FB: STA    GRP1
       STY    TMP_F5
       TXA
       TAY
       NOP
       NOP
       NOP
       NOP
       JMP    LF2CE
LF308: LDA    BirdGraphics_HMP0
       STA    HMP0
       STA    Unused_F9
       STA    Unused_F9
       LDX    #$47
       DEY
       CPY    #$15
       BCS    LF32A
       LDA    #$00
       STA    PF1
       LDA    (PropellerGraphicsPointer),Y
       STA    GRP1
       LDA    LFF45,Y
       NOP
       NOP
       NOP
       STA    PF1
       JMP    LF330
LF32A: LDA    #$00
       STA    PF1
       STA    GRP1
LF330: STA    WSYNC
       STA    HMOVE
       JSR    LFDBA
       JSR    LFDBC
       STA    HMCLR
       LDA    #$00
       STA    PF1
       INC    TMP_F5
       DEC    TMP_F5
       DEY
       CPY    #$15
       BCS    LF355
       LDA    (PropellerGraphicsPointer),Y
       STA    GRP1
       LDA    LFF45,Y
       STA    PF1
       JMP    LF35E
LF355: LDA    #$00
       STA    GRP1
       STA    PF1
       JMP    LF35E
LF35E: STA    WSYNC
       STA    HMOVE
       LDA    CurrentObjectIsBarnFlag
       BNE    LF3AC
       JSR    LFDBC
       LDA    Colors+2
       STA    COLUP0
       NOP
       NOP
       NOP
       NOP
LF371: DEY
       LDA    #$00
       STA    PF1
       CPY    #$15
       BCC    LF381
       STY    TMP_F5
       TXA
       TAY
       JMP    LF390
LF381: LDA    (PropellerGraphicsPointer),Y
       STA    GRP1
       LDA    LFF45,Y
       STY    TMP_F5
       STX    TMP_F6
       LDY    TMP_F6
       STA    PF1
LF390: STA    WSYNC
       STA    HMOVE
       CPX    #$39
       BCS    LF39E
       LDA    LFF5A,Y
       JMP    LF3A0
LF39E: LDA    (WindmillGraphicsPointer),Y
LF3A0: AND    CurrentObjectRenderFlag
       STA    GRP0
       LDY    TMP_F5
       DEX
       BPL    LF371
       JMP    LF483
LF3AC: JSR    LFDB9
       NOP
       NOP
       STA    Unused_F9
LF3B3: DEY
       LDA    #$00
       STA    PF1
       CPY    #$15
       BCC    LF3C5
       STA    GRP1
       STY    TMP_F5
       TXA
       TAY
       JMP    LF3D4
LF3C5: LDA    (PropellerGraphicsPointer),Y
       STA    GRP1
       LDA    LFF45,Y
       STY    TMP_F5
       STX    TMP_F6
       LDY    TMP_F6
       STA    PF1
LF3D4: STA    WSYNC
       STA    HMOVE
       LDA    Colors+5
       STA    COLUP0
       LDA    BirdGraphicsAnimationFrame0+1,Y
       AND    CurrentObjectRenderFlag
       STA    GRP0
       LDY    TMP_F5
       NOP
       NOP
       NOP
       CPX    #$30
       BEQ    LF40A
       DEX
       BPL    LF3B3
       NOP
       NOP
       NOP
       NOP
       NOP
       NOP
       NOP
       NOP
       NOP
       NOP
       NOP
       NOP
       NOP
       NOP
       NOP
       NOP
       NOP
       NOP
       NOP
       NOP
LF403: STA    GRP1
       STY    TMP_F5
       JMP    LF420
LF40A: DEX
LF40B: DEY
       LDA    #$00
       STA    PF1
       CPY    #$15
       BCS    LF403
       LDA    (PropellerGraphicsPointer),Y
       STA    GRP1
       LDA    LFF45,Y
       STY    TMP_F5
       NOP
       STA    PF1
LF420: TXA
       TAY
       LDA    #$07
       STA    WSYNC
       STA    HMOVE
       STA    NUSIZ0
       LDA    BarnColorsData,Y
       EOR    ColorsModifier1
       AND    ColorsModifier2
       STA    COLUP0
       LDA    BarnGraphicsUpperHalf
       STA    GRP0
       LDY    TMP_F5
       DEX
       CPX    #$18
       BCS    LF40B
       LDA    #$E0
       STA    HMP0
LF442: DEY
       LDA    #$00
       STA    PF1
       CPY    #$15
       BCS    LF478
       LDA    (PropellerGraphicsPointer),Y
       STA    GRP1
       LDA    LFF45,Y
       STY    TMP_F5
       STA    PF1
LF456: TXA
       TAY
       LDA    #$07
       NOP
       STA    HMOVE
       STA    NUSIZ0
       LDA    BarnColorsData,Y
       EOR    ColorsModifier1
       AND    ColorsModifier2
       STA    COLUP0
       LDA    BarnGraphicsLowerHalf
       STA    GRP0
       LDY    TMP_F5
       STA    HMCLR
       STA    Unused_F9
       DEX
       BPL    LF442
       JMP    LF483
LF478: STA    GRP1
       STY    TMP_F5
       NOP
       NOP
       NOP
       NOP
       JMP    LF456
LF483: LDA    CXPPMM ; collision between P0 (objects) and P1 (propeller)
       ORA    CXP0FB ; collision between P0 (objects) and PF (body of plane)
       ORA    ObjectCollisionFlag
       STA    ObjectCollisionFlag
       STA    CXCLR
       STA    WSYNC
       STA    HMOVE
       LDA    #$00
       STA    GRP1
       STA    GRP0
       LDA    Colors+2
       STA    COLUBK
       LDA    Colors+5
       STA    COLUP0
       STA    COLUP1
       LDA    #$06
       STA    NUSIZ0
       STA    NUSIZ1
       LDY    #$0C
LF4A9: CPY    #$07
       LDX    #$00
       BCS    LF4B1
       LDX    #$02
LF4B1: STA    WSYNC
       STA    HMOVE
       STX    ENAM0
       STX    ENAM1
       LDA    LF162,Y
       EOR    ColorsModifier1
       AND    ColorsModifier2
       STA    COLUBK
       DEY
       BPL    LF4A9
       LDY    #$02
LF4C7: LDA    #$00
       LDX    Colors+6
       STA    WSYNC
       STA    HMOVE
       STA    ENAM0
       STA    ENAM1
       STX    COLUBK
       DEY
       BPL    LF4C7
       STA    WSYNC
       STA    HMOVE
       LDA    Colors+1
       STA    COLUBK
       STA    WSYNC
       STA    HMOVE
       STA    Unused_F9
       LDX    #$00
       STX    HMCLR
       STX    REFP0
       STX    REFP1
       INX
       STX    NUSIZ0
       STA    Unused_F9
       STA    RESP0
       STA    RESP1
       INX
       INX
       STX    NUSIZ1
       LDA    #$10
       STA    HMP1
       LDY    #$07
LF501: LDA    (GameModeDigitPointer),Y
       STA    TMP_F5
       STA    WSYNC
       STA    HMOVE
       LDA    ActivisionLogo0,Y
       STA    GRP0
       LDA    ActivisionLogo1,Y
       STA    GRP1
       NOP
       LDA    ActivisionLogo3,Y
       TAX
       LDA    ActivisionLogo2,Y
       STA    GRP0
       STX    GRP1
       LDA    TMP_F5
       STA    GRP1
       STA    HMCLR
       DEY
       BPL    LF501
       INY
       STY    GRP0
       STY    GRP1
       LDA    #$21
       LDX    #$82
       STA    WSYNC
       STA    TIM64T
       STX    VBLANK
       LDA    #>DigitData0
       STA    BarnsLeftDigitPointers+1
       STA    BarnsLeftDigitPointers+3
       STA    GameModeDigitPointer+1
       LDA    NumberOfBarnsLeft
       AND    #$F0
       LSR
       BNE    LF549
       LDA    #<DigitDataA
LF549: STA    BarnsLeftDigitPointers+0
       LDX    GameMode
       INX
       TXA
       ASL
       ASL
       ASL
       STA    GameModeDigitPointer
       LDA    NumberOfBarnsLeft
       AND    #$0F
       ASL
       ASL
       ASL
       STA    BarnsLeftDigitPointers+2
       LDY    #$08
       LDX    #$02
LF561: LDA    InGameTime,X
       AND    #$0F
       ASL
       ASL
       ASL
       STA    InGameTimeDisplayPointers,Y
       DEX
       BMI    LF57C
       DEY
       DEY
       LDA    InGameTime+1,X ; skip minutes
       AND    #$F0
       LSR
       STA    InGameTimeDisplayPointers,Y
       DEY
       DEY
       BPL    LF561
LF57C: CLD
       LDA    PlayerDisabled
       BNE    LF585             ; does nothing
       LDA    InactivityTimer
       BMI    LF585             ; does nothing
LF585: LDA    GlobalFrameCounter
       AND    #$07
       BNE    LF599
       LDX    BirdGraphicsAnimationCycle
       INX
       TXA
       AND    #$07
       STA    BirdGraphicsAnimationCycle
       TAY
       LDA    BirdGraphicsAnimationPointerData,Y
       STA    BirdGraphicsAnimationLowAddr
LF599: LDY    #<BirdGraphicsAnimationFrame0
       LDA    GlobalFrameCounter
       AND    #$01
       BNE    LF5A3
       LDY    #<BirdGraphicsAnimationFrame2
LF5A3: STY    BirdGraphicsStartledAnimationLowAddr
       LDX    #$05
LF5A7: LDY    #$00
       LDA    InactivityTimer
       CMP    #$20
       BCS    LF5B5
       LDA    PlayerDisabled
       BNE    LF5B5
       LDY    AudioControlRegisters,X
LF5B5: STY    AUDC0,X
       DEX
       BPL    LF5A7
       LDA    CollisionCooldownTimerDoubled
       BNE    LF5CC
       LDA    PlayerSpeed
       CMP    #$0A
       BCS    LF5CC
       LDA    PlayerYPosition
       CMP    #$7D
       BCS    LF5CC
       INC    PlayerYPosition
LF5CC: LDA    INTIM
       BNE    LF5CC
       LDY    #$82
       STY    WSYNC
       STY    VSYNC
       STY    WSYNC
       STY    WSYNC
       STY    WSYNC
       STA    VSYNC

       INC    GlobalFrameCounter ;; start of frame
       BNE    LF5EA
       INC    InactivityTimer
       BNE    LF5EA
       SEC
       ROR    InactivityTimer
LF5EA: LDY    #$FF
       LDA    SWCHB
       AND    #$08
       BNE    ColorSwitchSetToColor
       LDY    #$0F
ColorSwitchSetToColor:
       TYA
       LDY    #$00
       BIT    InactivityTimer
       BPL    LF600
       AND    #$F7
       LDY    InactivityTimer
LF600: STY    ColorsModifier1
       ASL    ColorsModifier1
       STA    ColorsModifier2
       LDA    #$30
       STA    WSYNC
       STA    TIM64T
       LDA    SWCHA ; joystick inputs
       TAY
       LDX    CollisionCooldownTimer
       BEQ    LF617
       LDY    #$0F ; likely oversight, enables all joystick directions instead of none, meant to be $ff
LF617: TYA
       LSR
       LSR
       LSR
       LSR
       STA    JoystickInputs
       INY
       BEQ    NoJoystickInputs
ButtonActivityDetected:
       LDA    #$00
       STA    InactivityTimer
       JMP    CheckForReset
NoJoystickInputs:
       BIT    INPT4
       BPL    ButtonActivityDetected
CheckForReset:
       LDA    SWCHB
       LSR
       BCS    ResetButtonNotPressed
       LDX    #$A8
       JMP    StartupClearMem  ; only clear RAM from $a8 onwards with this reset
ResetButtonNotPressed:
       LDY    #$00
       LSR
       BCS    SelectButtonNotPressed
       LDA    GameModeSwitchCooldown
       BEQ    SwitchGameMode
       DEC    GameModeSwitchCooldown
       BPL    GameModeSwitchCooldownNotExpired
SwitchGameMode:
       INC    GameMode
       LDA    GameMode
       CMP    #$03
       BNE    ColdBoot
       JSR    CycleRNG82
ColdBoot:
       LDA    GameMode
       AND    #$03
       STA    GameMode
       STA    InactivityTimer
       LDA    #$AA
       STA    NumberOfBarnsLeft
       STA    InGameTime+0
       STA    InGameTime+1
       STA    InGameTime+2
       STA    InGameTime+3
       LDA    #<DigitDataA
       STA    InGameTimeDisplayPointers+2 ; lower byte of ten seconds digit graphics pointer, used to show/hide decimal points
       LDY    #$1E
       STY    PlayerDisabled
SelectButtonNotPressed:
       STY    GameModeSwitchCooldown
GameModeSwitchCooldownNotExpired:
       LDA    InactivityTimer
       BMI    MainGameLoop_
       LDA    PlayerDisabled
       BEQ    ProcessActivePlayer
MainGameLoop_:
       JMP    MainGameLoop

ProcessActivePlayer:
       LDA    GameStartedFlag
       BNE    GameIsAlreadyRunning
       BIT    INPT4
       BPL    StartGame
       JMP    MainGameLoop
StartGame:
       LDA    #$01
       STA    GameStartedFlag
       STA    GlobalFrameCounter
GameIsAlreadyRunning:
       BIT    GameWonFlag
       BPL    GameStillOngoing
       LDA    BirdPosition+2
       ORA    BirdPosition+0
       ORA    BirdPosition+1
       BNE    LF6A1
       LDA    PlayerYPosition
       CMP    #$7D
       BNE    LF6A1
       LDA    #<PropellerGraphicsDataStanding
       STA    PropellerGraphicsPointer
       INC    PlayerDisabled
LF6A1: LDA    GlobalFrameCounter
       LDX    PlayerSpeed
       BEQ    LF6BA
       CPX    #$1A
       BCS    LF6B4
       AND    #$03
       BNE    LF6BA
       DEC    PlayerSpeed
       JMP    LF6BA
LF6B4: AND    #$01
       BNE    LF6BA
       DEC    PlayerSpeed
LF6BA: LDA    GlobalFrameCounter
       AND    #$03
       BNE    LF6C6
       LDA    EngineSoundPitch
       BEQ    LF6C6
       DEC    EngineSoundPitch
LF6C6: JMP    SkipVerticalSpeedUpdates
GameStillOngoing:
       LDA    InGameTime+0 ; minutes
       CMP    #$05
       BNE    TimeNotUpYet
       INC    PlayerDisabled
       JMP    SkipVerticalSpeedUpdates
TimeNotUpYet:
       CLC
       LDA    InGameTime+3 ; 1/25600th seconds
       ADC    #$AB         ; add 171/25600 = 0.0066796875 seconds
       STA    InGameTime+3 ; 1/25600th seconds
       LDA    #$01         ; add 0.01 seconds
       SED
       ADC    InGameTime+2 ; hundreds
       STA    InGameTime+2 ; hundreds
       LDA    InGameTime+1 ; seconds
       ADC    #$00
       STA    InGameTime+1 ; seconds
       CLD
       CMP    #$60
       BCC    NoSecondsOverflow
       LDA    #$00
       STA    InGameTime+1 ; seconds
       INC    InGameTime+0 ; minutes
NoSecondsOverflow:
       LDA    GlobalFrameCounter
       AND    #$01
       BNE    SkipSpeedUpdates
       BIT    INPT4
       BMI    Speed_NotPressingB
       LDX    PlayerSpeed
       INX
       CPX    #$25
       BCC    NotAtMaxSpeedYet
       LDX    #$25
NotAtMaxSpeedYet:
       STX    PlayerSpeed
       LDA    GlobalFrameCounter
       AND    #$07
       BNE    DontIncreaseEnginePitch
       INC    EngineSoundPitch
DontIncreaseEnginePitch:
       JMP    SkipSpeedUpdates
Speed_NotPressingB:
       LDA    GlobalFrameCounter
       AND    #$07
       BNE    DontDecreaseEnginePitch
       DEC    EngineSoundPitch
DontDecreaseEnginePitch:
       LDX    PlayerSpeed
       CPX    #$18
       BCS    PlayerSpeedAtLeast18
       INC    PlayerSpeed            ; make player speed revert to default speed of $18
       JMP    SkipSpeedUpdates
PlayerSpeedAtLeast18:
       BEQ    SkipSpeedUpdates
       DEC    PlayerSpeed
SkipSpeedUpdates:
       LDA    EngineSoundPitch       ; keep engine pitch between 0 and 7
       BPL    EnginePitchNonnegative
       LDA    #$00
EnginePitchNonnegative:
       CMP    #$08
       BCC    EnginePitchNotTooHigh
       LDA    #$07
EnginePitchNotTooHigh:
       STA    EngineSoundPitch
       LDX    #$00
       LDA    GlobalFrameCounter
       AND    #$01
       BNE    SkipVerticalSpeedUpdates
       LDA    JoystickInputs
       EOR    #$0F
       AND    #$03
       BEQ    NoUpDownPressed
       LDA    PlayerVerticalSpeed
       CLC
       ADC    #$05
       CMP    #$17
       BCC    VerticalSpeedNotTooHigh
       LDA    #$17
VerticalSpeedNotTooHigh:
       TAX
NoUpDownPressed:
       STX    PlayerVerticalSpeed
SkipVerticalSpeedUpdates:
       NOP

       LDA    ActiveBirdLane
       LDX    #$03
UpdateBirdLanesLoop:
       STA    TMP_F5               ; Active bird lanes
       AND    #$01
       BEQ    SkipToNextBirdLane   ; no active birds on current lane
       LDA    BirdPosition-1,X
       BNE    BirdAlreadyFlying
       LDA    #$A0                 ; initialize new bird to position $a0
BirdAlreadyFlying:
       LDY    PlayerDisplacement+1 ; horizontal displacement
       DEY
       STY    TMP_F6 ; relative player displacement to bird, birds fly at 1 unit/s
       SEC
       SBC    TMP_F6
       STA    TMP_F7 ; new bird's position after subtracting relative player displacement
       BEQ    BirdOutsideScreenArea
       CMP    #$A1
       BCS    BirdOutsideScreenArea
       LDA    BirdGraphics_NumberSize-1,X
       AND    #$F0
       STA    BirdGraphics_NumberSize-1,X
       LDY    BirdFormation-1,X
       LDA    TMP_F7
       CMP    BirdPositionUntilSecondBirdData,Y
       BCS    SetNewBirdPosition
       LDA    BirdGraphics_NumberSize-1,X
       ORA    BirdFormation-1,X
       STA    BirdGraphics_NumberSize-1,X
       LDA    TMP_F7
       JMP    SetNewBirdPosition
LF791:  LDA    BirdGraphics_NumberSize-1,X ;
        ORA    BirdFormation-1,X           ;
        STA    BirdGraphics_NumberSize-1,X ; Unused duplicate code
        LDA    TMP_F7                      ;
        JMP    SetNewBirdPosition          ;
BirdOutsideScreenArea:
       LDA    BirdGraphics_NumberSize-1,X
       AND    #$0F
       CMP    #$00
       BEQ    NoSecondBirdOnLane
       TAY
       LDA    SecondBirdRelativePositionData,Y ; convert formation to single bird at second bird's position
       STA    BirdPosition-1,X
       LDA    BirdGraphics_NumberSize-1,X
       AND    #$F0
       STA    BirdGraphics_NumberSize-1,X
       LDA    #$00
       STA    BirdFormation-1,X
       JMP    SkipToNextBirdLane
NoSecondBirdOnLane:
       INC    BirdsPassedCounter-1,X
       LDA    BirdsPassedCounter-1,X
       AND    #$03
       STA    BirdsPassedCounter-1,X
       TAY
       STX    TMP_F8   ; current lane index
       LDX    GameMode
       LDA    #>BirdFormationData0
       STA    PTR_98+1
       LDA    BirdFormationPointerData,X
       STA    PTR_98
       LDA    (PTR_98),Y
       LDX    TMP_F8
       STA    BirdFormation-1,X ; set next formation if lane becomes active again in the future
       LDA    LaneIndexToActiveBirdLaneFlagMap,X
       EOR    ActiveBirdLane
       STA    ActiveBirdLane    ; clear active bird lane flag to disable lane
       LDA    #$00
SetNewBirdPosition:
       STA    BirdPosition-1,X
SkipToNextBirdLane:
       LDA    TMP_F5
       LSR
       DEX
       BEQ    DoneUpdatingBirdLanes
       JMP    UpdateBirdLanesLoop
DoneUpdatingBirdLanes:

       LDA    PlayerDisabled
       BNE    SkipUpdatingCyclingGraphics
       LDA    GlobalFrameCounter  ; update propeller graphics cycle
       AND    #$01
       BNE    LF7FD
       BIT    INPT4
       BPL    LF7FB
       LDA    GlobalFrameCounter
       AND    #$03
       BNE    LF7FD
LF7FB: INC    PropellerGraphicsCycle
LF7FD: LDX    PropellerGraphicsCycle
       CPX    #$03
       BCC    LF807
       LDX    #$00
       STX    PropellerGraphicsCycle
LF807: LDA    PropellerGraphicsPointerData,X
       STA    PropellerGraphicsPointer
       LDA    GlobalFrameCounter  ; update windmill graphics cycle
       AND    #$03
       BNE    LF814
       INC    WindmillGraphicsCycle
LF814: LDX    WindmillGraphicsCycle
       CPX    #$03
       BCC    LF81C
       LDX    #$00
LF81C: STX    WindmillGraphicsCycle
       LDA    WindmillGraphicsPointerData,X
       STA    WindmillGraphicsPointer
       LDA    #>WindmillGraphicsData0
       STA    WindmillGraphicsPointer+1
SkipUpdatingCyclingGraphics:

       LDX    CollisionCooldownTimerDoubled
       BEQ    LF82E
       DEX
       STX    CollisionCooldownTimerDoubled
LF82E: LDX    BarnScoreCooldownTimer
       BEQ    LF835
       DEX
       STX    BarnScoreCooldownTimer

LF835: LDX    #$03
LF837: LDA    BirdGraphicsAnimationLowAddr ; initialize all bird lanes as non-startled
       STA    BirdGraphicsLaneAnimationLowAddr-1,X
       DEX
       BNE    LF837
       STX    AudioControlRegisters+5 ; audio volume 1
       LDA    #$1F
       SEC
       SBC    EngineSoundPitch
       STA    AudioControlRegisters+2 ; audio frequency 0
       LDA    #$0A
       STA    AudioControlRegisters+0 ; audio control 0
       LDA    GlobalFrameCounter
       AND    #$03
       BNE    LF85D
       INC    EngineSoundVolume
       LDA    EngineSoundVolume
       CMP    #$03
       BCC    LF85B
       LDA    #$03
LF85B: STA    EngineSoundVolume
LF85D: LDA    EngineSoundVolume
       TAY
       BIT    INPT4 ; increase engine sound volume if pressing B
       BMI    LF865
       INY
LF865: STY    AudioControlRegisters+4 ; audio volume 0
       LDY    CurrentObjectIndex
       LDA    #>ObjectData0
       STA    PTR_98+1
       LDX    GameMode
       LDA    ObjectDataPointers,X
       STA    PTR_98
       LDA    (PTR_98),Y
       STA    TMP_F5 ; current object
       LDA    CurrentObjectPosition ; increase audio volume if currently passing over object
       CMP    #$40
       BCS    NotPassingOverObject
       LDY    #$0A
       STY    AudioControlRegisters+1 ; audio control 1
       LDY    #$15
       STY    AudioControlRegisters+3 ; audio frequency 1
       CMP    #$20
       BCC    LF88E
       DEC    AudioControlRegisters+3 ; audio frequency 1
       EOR    #$3F
LF88E: LSR
       LDY    PlayerYPosition ; or muffle sound if flying through barn
       CPY    #$55
       BCC    LF897
       LDA    #$00
LF897: AND    CurrentObjectRenderFlag
       STA    AudioControlRegisters+5 ; audio volume 1
NotPassingOverObject:

       LDX    CollisionCooldownTimer
       BEQ    CheckForCollisions
       DEX
       STX    CollisionCooldownTimer
       JMP    HandleOngoingObjectCollision ; still in collision animation
CheckForCollisions:
       DEX
       STX    CollisionDeflectionDirection ; clear collision direction
       BIT    ObjectCollisionFlag
       BMI    HandleNewObjectCollision
       JMP    ObjectCollisionHandlingDone ; no collisions, done
HandleNewObjectCollision:
       LDA    TMP_F5
       LSR
       BCS    NewObjectCollision ; collided with windmill
       LDA    BarnEndPosition
       SEC
       SBC    #$18
       CMP    #$22
       BCS    LF8C5
       LDA    #$01                    ; muffle sound if flying through barn
       STA    AudioControlRegisters+4 ; audio volume 0
       LDA    #$00
       STA    AudioControlRegisters+5 ; audio volume 1
LF8C5: LDA    BarnEndPosition
       CMP    #$0C
       BCC    ScoreBarn ; already passed through, no need to check Y Position
       LDA    #$64
       BIT    SWCHB
       BVC    BarnCollision_BeginnerDifficulty
       LDA    #$79
BarnCollision_BeginnerDifficulty:
       STA    TMP_F8
       LDA    PlayerYPosition
       CMP    TMP_F8
       BCC    NewObjectCollision ; y position needs to be >= $64 (beginner) or >=$79 (advanced) to pass through
ScoreBarn:
       LDY    BarnScoreCooldownTimer
       BNE    ObjectCollisionHandlingDone_
       LDA    #$50
       STA    BarnScoreCooldownTimer
       LDA    BarnEndPosition
       CMP    #$30
       BCC    ObjectCollisionHandlingDone_ ; only score at beginning of barn
       LDA    NumberOfBarnsLeft
       BEQ    ObjectCollisionHandlingDone_
       SED
       SEC
       SBC    #$01
       STA    NumberOfBarnsLeft
       BNE    ObjectCollisionHandlingDone_
       DEC    GameWonFlag
ObjectCollisionHandlingDone_:
       CLD
       JMP    ObjectCollisionHandlingDone
NewObjectCollision:
       LDA    #$46
       STA    CollisionCooldownTimerDoubled
       LSR
       STA    CollisionCooldownTimer
HandleOngoingObjectCollision:
       LDA    #$08
       STA    AudioControlRegisters+1 ; audio control 1
       LDX    #$00
       STX    AudioControlRegisters+4 ; audio volume 0
       STX    EngineSoundPitch
       STX    ObjectCollisionFlag
       LDA    CollisionCooldownTimer
       LSR
       STA    AudioControlRegisters+5 ; audio volume 1
       LDA    CollisionDeflectionDirection
       BMI    DetermineCollisionDirection
       LDA    PlayerYPosition
       LDX    CollisionDeflectionDirection
       CPX    #$01
       BEQ    DeflectPlayerDownwards
       CLC
       ADC    #$04
       TAX
       LDA    GlobalFrameCounter ; alternating down 4 and up 5
       AND    #$01
       BNE    LF92F
       TXA
       SEC
       SBC    #$09
       TAX
LF92F: JMP    ApplyPlayerYDeflection
DeflectPlayerDownwards:
       CLC
       ADC    #$05
       TAX
       LDA    GlobalFrameCounter ; alternating down 5 and up 1
       AND    #$01
       BNE    ApplyPlayerYDeflection
       TXA
       SEC
       SBC    #$06
       CMP    #$64               ; but never above $64 (add 2 more downwards in that case)
       BCS    LF947
       CLC
       ADC    #$02
LF947: TAX
ApplyPlayerYDeflection:
       STX    PlayerYPosition
       LDA    CollisionDeflectionDirection
       BMI    DetermineCollisionDirection
       BEQ    CollisionDirection0
       LSR
       BCS    CollisionDirection1
       JMP    CollisionDirection2
DetermineCollisionDirection:
       LDA    TMP_F5
       LSR
       BCS    WindmillCollision
       LDA    BarnEndPosition
       CMP    #$39
       BCS    CollisionDirection2
       LDA    PlayerYPosition
       CMP    #$5A
       BCC    CollisionDirection0
CollisionDirection1:
       LDA    #$01
       STA    CollisionDeflectionDirection
       LDA    #$01
       STA    PlayerDisplacement+1 ; horizontal displacement
       LDA    #$04
       STA    PlayerSpeed
       JMP    ObjectCollisionHandlingDone
CollisionDirection0:
       LDA    #$01
       STA    PlayerDisplacement+1 ; horizontal displacement
       LDA    #$04
       STA    PlayerSpeed
       LDA    #$00
       STA    CollisionDeflectionDirection
       JMP    ObjectCollisionHandlingDone
WindmillCollision:
       LDA    CurrentObjectPosition
       CMP    #$20
       BCC    CollisionDirection0
CollisionDirection2:
       LDX    #$00
       STX    PlayerSpeed
       DEX
       STX    PlayerDisplacement+1 ; horizontal displacement
       LDA    #$02
       STA    CollisionDeflectionDirection
       JMP    ObjectCollisionHandlingDone
ObjectCollisionHandlingDone:

       LDX    #$03
CheckBirdLaneCollisionLoop:
       LDY    BirdLaneCollisionTimer-1,X
       BEQ    CheckForNewBirdLaneCollision ; no ongoing collision, check for new one
       DEY
       STY    BirdLaneCollisionTimer-1,X
       CPY    #$27
       BCC    SkipPlayingBirdBumpSound
       LDA    #$0C
       STA    AudioControlRegisters+1 ; audio control 1
       LDA    #$1F
       STA    AudioControlRegisters+3 ; audio frequency 1
       LDA    #$05
       STA    AudioControlRegisters+5 ; audio volume 1
SkipPlayingBirdBumpSound:
       LDY    BirdPosition-1,X
       BEQ    CheckNextBirdLaneCollision
       INY
       INY
       STY    BirdPosition-1,X  ; move startled bird 2px forward
       LDA    BirdGraphicsStartledAnimationLowAddr
       STA    BirdGraphicsLaneAnimationLowAddr-1,X
       LDA    #$00
       STA    BirdLaneCollisionFlag-1,X
       JMP    CheckNextBirdLaneCollision
CheckForNewBirdLaneCollision:
       LDA    BirdLaneCollisionFlag-1,X
       BPL    CheckNextBirdLaneCollision
       LDA    #$00
       STA    EngineSoundPitch
       LDA    #$10
       STA    PlayerSpeed
       LDA    #$30
       STA    BirdLaneCollisionTimer-1,X
CheckNextBirdLaneCollision:
       DEX
       BNE    CheckBirdLaneCollisionLoop

       LDX    #$01
       LDA    CollisionCooldownTimer
       BEQ    UpdatePlayerDisplacementLoop ; during object collision, only update vertical displacement
       LDX    #$00
UpdatePlayerDisplacementLoop:
       LDA    PlayerVerticalSpeed,X ; or horizontal speed
       CPX    #$01
       BNE    UpdateVerticalDisplacement
       LDA    JoystickInputs
       AND    #$03
       CMP    #$03
       LDA    PlayerSpeed
       BCS    UpdateVerticalDisplacement
       LSR
       LSR
       LSR
       LSR
       STA    TMP_F5
       SEC
       LDA    PlayerSpeed  ; if pressing up or down, decrease horizontal displacement by 1/16th
       SBC    TMP_F5
UpdateVerticalDisplacement:
       STA    TMP_F6 ; speed (maybe adjusted)
       LSR
       LSR
       LSR
       LSR
       STA    PlayerDisplacement,X
       LDA    TMP_F6
       AND    #$0F
       CLC
       ADC    PlayerDisplacementFractional,X
       CMP    #$10
       BCC    NoFractionalDisplacementOverflow
       INC    PlayerDisplacement,X
NoFractionalDisplacementOverflow:
       AND    #$0F
       STA    PlayerDisplacementFractional,X
       DEX
       BPL    UpdatePlayerDisplacementLoop

       BIT    GameWonFlag
       BMI    SkipUpdatingPlayerYPosition
       LDA    JoystickInputs
       AND    #$02
       BNE    NotPressingDown
       LDA    PlayerYPosition
       CLC
       ADC    PlayerDisplacement+0 ; vertical displacement
       STA    PlayerYPosition
NotPressingDown:
       LDA    JoystickInputs
       AND    #$01
       BNE    NotPressingUp
       LDA    PlayerYPosition
       SEC
       SBC    PlayerDisplacement+0 ; vertical displacement
       STA    PlayerYPosition
NotPressingUp:
       LDA    PlayerYPosition
       CMP    #$14
       BCS    LFA3D
       LDA    #$14
LFA3D: CMP    #$7E
       BCC    LFA43
       LDA    #$7D
LFA43: STA    PlayerYPosition ; ensure player y position stays between $14 and $7d
SkipUpdatingPlayerYPosition:

       LDA    FencePostGraphicsPosition
       SEC
       SBC    PlayerDisplacement+1 ; horizontal displacement
       STA    TMP_F5
       CMP    #$A0
       BNE    FencePositionNoOverflow
       LDA    #$00
       JMP    FencePositionNoUnderflow
FencePositionNoOverflow:
       CMP    #$F0
       BCC    FencePositionNoUnderflow
       SEC
       LDA    #$00
       SBC    TMP_F5
       STA    TMP_F5
       SEC
       LDA    #$A0
       SBC    TMP_F5
FencePositionNoUnderflow:
       STA    FencePostGraphicsPosition

       LDA    CurrentObjectIsBarnFlag
       BEQ    UpdateWindmillObjectPosition ; not a barn
       SEC
       LDA    BarnEndPosition
       SBC    PlayerDisplacement+1 ; horizontal displacement
       CMP    #$F0
       BCC    NoBarnEndUnderflow
       LDA    #$00
       STA    CurrentObjectPosition
NoBarnEndUnderflow:
       STA    BarnEndPosition
       BNE    BarnNotYetOffscreen
       JMP    ObjectLeftScreen
BarnNotYetOffscreen:
       JMP    UpdateBarnPositionVariables
UpdateWindmillObjectPosition:
       SEC
       LDA    CurrentObjectPosition
       SBC    PlayerDisplacement+1 ; horizontal displacement
       CMP    #$F0
       BCC    NoWindmillPositionUnderflow
       LDA    #$00
NoWindmillPositionUnderflow:
       STA    CurrentObjectPosition
       BEQ    ObjectLeftScreen
       JMP    UpdateObjectPositionDone
ObjectLeftScreen:
       LDX    #$00
       STX    ObjectCollisionFlag
       DEX
       STX    CurrentObjectRenderFlag ; set to $ff
       INC    CurrentObjectIndex
       LDA    CurrentObjectIndex
       AND    #$3F
       STA    CurrentObjectIndex
       TAY
       LDA    #>ObjectData0
       STA    PTR_98+1
       LDX    GameMode
       LDA    ObjectDataPointers,X
       STA    PTR_98
       LDA    (PTR_98),Y
       BEQ    NextObjectEmpty
       LSR
       BCS    NextObjectWindmill
       JMP    NextObjectBarn
NextObjectEmpty:
       LDA    #$00
       STA    CurrentObjectIsBarnFlag
       STA    CurrentObjectRenderFlag
       LDA    #$9F
       STA    CurrentObjectPosition
       JMP    UpdateObjectPositionDone
NextObjectWindmill: LDX    #$00
       STX    CurrentObjectIsBarnFlag
       DEX
       STX    CurrentObjectRenderFlag
       LDA    #$9F
       STA    CurrentObjectPosition
       JMP    UpdateObjectPositionDone
NextObjectBarn:
       STA    CurrentObjectIsBarnFlag
       LDA    #$B7
       STA    BarnEndPosition
       JMP    UpdateBarnPositionVariables
UpdateBarnPositionVariables:
       LDA    #$FF
       STA    BarnGraphicsUpperHalf
       LDA    #$FE
       STA    BarnGraphicsLowerHalf
       LDA    BarnEndPosition
       CMP    #$A0
       BCC    BarnNotOffscreenRight
       SBC    #$A0
       LSR
       LSR
       TAX
       JMP    ApplyOffscreenMask
BarnNotOffscreenRight:
       CMP    #$18
       BCC    BarnOffscreenLeft
       LDX    #$06
       JMP    ApplyOffscreenMask
BarnOffscreenLeft:
       LDX    #$00
       STX    CurrentObjectRenderFlag ; disable weather vane rendering
       LSR
       LSR
       CLC
       ADC    #$07
       TAX
ApplyOffscreenMask:
       LDY    #$01
ApplyOffscreenMaskLoop:
       LDA    BarnGraphicsUpperHalf,Y
       AND    BarnGraphicsOffscreenMaskData,X
       STA    BarnGraphicsUpperHalf,Y
       DEY
       BPL    ApplyOffscreenMaskLoop
       LDA    BarnEndPosition
       SEC
       SBC    #$18
       CMP    #$E0
       BCC    BarnObjectPositionNoUnderflow
       SEC
       SBC    #$60
BarnObjectPositionNoUnderflow:
       STA    CurrentObjectPosition
UpdateObjectPositionDone:

       LDX    PlayerSpeed
       CPX    #$20
       BCC    MainGameLoop__
       LDA    #$3F
       BIT    SWCHB
       BPL    BirdDifficultyBeginner
       LSR
BirdDifficultyBeginner:
       STA    TMP_F5 ; $3f for beginner, $1f for advanced
       LDA    GlobalFrameCounter
       AND    TMP_F5
       BNE    DontIncreaseBirdLaneCycle
       INC    NextBirdLaneCycle
DontIncreaseBirdLaneCycle:
       LDA    NextBirdLaneCycle
       AND    #$0F
       STA    NextBirdLaneCycle
       TAY
       LDX    GameMode
       LDA    #>BirdLaneData0
       STA    PTR_98+1
       LDA    BirdLanePointerData,X
       STA    PTR_98
       LDA    (PTR_98),Y
       ORA    ActiveBirdLane
       STA    ActiveBirdLane
MainGameLoop__:
       JMP    MainGameLoop
UnusedNops: .byte $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA
            .byte $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA
            .byte $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA
            .byte $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA
            .byte $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA
            .byte $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA
            .byte $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA
            .byte $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA
            .byte $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA
            .byte $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA
            .byte $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA
ObjectData0: .byte $03,$03,$02,$03,$02,$03,$02,$03,$00,$03,$02,$03,$03,$03,$02,$03
             .byte $02,$03,$03,$02,$02,$03,$02,$03,$03,$03,$02,$00,$02,$02,$02,$00
             .byte $02,$02,$02,$00,$02,$02 ; has $1a overlap with ObjectData1
ObjectData1: .byte $03,$00,$03,$03,$03,$02,$02,$02,$03,$02,$03,$02,$03,$02,$03,$03
             .byte $03,$02,$03,$03,$03,$02,$03,$03,$03,$03,$03,$03,$02,$02,$03,$03
             .byte $03,$02,$03,$02,$03,$02,$03,$02,$02,$03,$03,$03,$02,$03,$03,$03
             .byte $02,$02,$02,$02,$02,$03,$03,$03,$02,$03,$03,$03,$02,$03,$03,$03
UnusedObjectData: .byte $02,$03,$03,$03,$02,$03 ; not actually part of any game mode
ObjectData2: .byte $03,$02,$02,$03,$03,$02,$02,$03,$03,$03,$02,$03,$03,$03,$02,$03
             .byte $03,$03,$03,$02,$02,$03,$02,$00,$03,$03,$02,$02,$03,$03,$02,$03
             .byte $03,$03,$02,$03,$03,$03,$02,$03,$03,$03,$02,$03,$03,$03,$02,$03
             .byte $03,$03,$02,$03,$03,$03,$02,$03,$03,$03,$02,$03,$02,$02,$02,$02
ObjectData3: .byte $00,$03,$02,$03,$02,$03,$03,$03,$00,$02,$03,$03,$02,$03,$02,$02
             .byte $03,$03,$03,$03,$02,$03,$02,$02,$03,$03,$02,$03,$02,$02,$03,$03
             .byte $02,$03,$02,$03,$02,$02,$02,$03,$00,$03,$03,$03,$02,$03,$02,$02
             .byte $00,$02,$02,$03,$03,$03,$03,$02 ; reads over for 8 bytes
ObjectDataPointers: .byte <ObjectData0,<ObjectData1,<ObjectData2,<ObjectData3 ; $fcXX pointer by game mode
BirdLanePointerData: .byte <BirdLaneData0,<BirdLaneData0,<BirdLaneData1,BirdLaneData2 ; $fdXX pointer by game mode
BirdFormationPointerData: .byte <BirdFormationData0,<BirdFormationData0,<BirdFormationData1,<BirdFormationData2 ; $fdXX addresses to bird formation data, by game mode
LaneIndexToActiveBirdLaneFlagMap: .byte $00,$04,$02,$01 ; maps land index ($3-$1) to corresponding flag bit in ActiveBirdLane
BarnGraphicsOffscreenMaskData: .byte $FC,$F8,$F0,$E0,$C0,$80,$FF,$03,$07,$0F,$1F,$3F,$7F
SecondBirdRelativePositionData: .byte $00,$10,$20,$20,$40 ; distance between first and second bird in each formation
BirdPositionUntilSecondBirdData: .byte $00,$90,$80,$80,$60 ; bird position when second bird spawns for each formation
BirdFormationData0: .byte $04,$04,$04,$04 ; formation of next bird on a lane, cycled through
BirdFormationData1: .byte $04,$04,$04,$04
BirdFormationData2: .byte $04,$00,$02,$04
BirdLaneData0: .byte $00,$01,$02,$04,$05,$02,$01,$02,$01,$04,$02,$01,$04,$02,$01,$02
BirdLaneData1: .byte $00,$02,$02,$04,$04,$02,$02,$02,$02,$04,$01,$01,$04,$02,$01,$02
BirdLaneData2: .byte $00,$01,$02,$04,$05,$02,$01,$02,$01,$04,$02,$01,$04,$02,$01,$02
SetupInitialGameState:
       LDA    #<BirdGraphicsAnimationFrame0
       STA    BirdGraphicsAnimationPointer
       STA    BirdGraphicsAnimationLowAddr
       LDA    #>BirdGraphicsAnimationFrame0
       STA    BirdGraphicsAnimationPointer+1
       LDA    #<PropellerGraphicsDataStanding
       STA    PropellerGraphicsPointer
       LDA    #>PropellerGraphicsDataStanding
       STA    PropellerGraphicsPointer+1
       LDA    #>DigitData0
       STA    InGameTimeDisplayPointers+1
       STA    InGameTimeDisplayPointers+3
       STA    InGameTimeDisplayPointers+5
       STA    InGameTimeDisplayPointers+7
       STA    InGameTimeDisplayPointers+9
       STA    BarnsLeftDigitPointers+1
       STA    BarnsLeftDigitPointers+3
       LDA    #$00
       LDX    GameMode
       CPX    #$03
       BNE    NotGameMode4
       LDA    RNG_82
       AND    #$3F        ; randomize starting object index and bird lane cycle for game mode 4
NotGameMode4:
       STA    CurrentObjectIndex
       AND    #$0F
       STA    NextBirdLaneCycle
       LDA    NumberOfBarnsData,X
       STA    NumberOfBarnsLeft
       LDA    #$7D
       STA    PlayerYPosition
       LDA    #$01
       STA    BirdsPassedCounter+0
       STA    BirdsPassedCounter+1
       STA    BirdsPassedCounter+2
       LDX    #$03
LFD8E: LDA    #$00
       STA    BirdGraphicsAnimationLowAddr,X ; also clears BirdGraphicsStartledAnimationLowAddr, is barn flag, and object render flag
       DEX
       BPL    LFD8E
       RTS

LFD96: JSR    LFDBD
       STA    WSYNC
       STA    HMOVE
       RTS

LFD9E: CLC
       ADC    #$2E
       TAY
       AND    #$0F
       STA    TMP_F5
       TYA
       LSR
       LSR
       LSR
       LSR
       TAY
       CLC
       ADC    TMP_F5
       CMP    #$0F
       BCC    LFDB6
       SBC    #$0F
       INY
LFDB6: EOR    #$07
       ASL
LFDB9: ASL
LFDBA: ASL
       ASL
LFDBC: RTS

LFDBD: JSR    LFD9E
       STA    HMP0,X
       STA    WSYNC
LFDC4: DEY
       BPL    LFDC4
       STA    RESP0,X
       RTS

CycleRNG82:
       LDA    RNG_82
       ASL
       ASL
       ASL
       EOR    RNG_82
       ASL
       ROL    RNG_82
       LDA    RNG_82
       RTS

LFDD7: .byte $00,$01,$01,$00,$00
LFDDC: .byte $01,$01,$00,$00,$00,$00,$00,$00
LFDE4: .byte $FF,$FF,$FF,$FF,$7F,$1F,$0E,$04
LFDEC: .byte $7F,$7F,$3F,$0F,$03,$00,$00,$00
       .byte $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA
DigitData0: .byte $78,$CC,$CC,$CC,$CC,$CC,$CC,$78 ; digit 0
DigitData1: .byte $78,$30,$30,$30,$30,$30,$70,$30 ; digit 1
DigitData2: .byte $FC,$C0,$C0,$78,$0C,$0C,$8C,$78 ; digit 2
DigitData3: .byte $78,$8C,$0C,$18,$18,$0C,$8C,$78 ; digit 3
DigitData4: .byte $18,$18,$18,$FC,$98,$58,$38,$18 ; digit 4
DigitData5: .byte $F8,$8C,$0C,$0C,$F8,$C0,$C0,$FC ; digit 5
DigitData6: .byte $78,$CC,$CC,$CC,$F8,$C0,$C4,$78 ; digit 6
DigitData7: .byte $30,$30,$30,$30,$18,$0C,$84,$FC ; digit 7
DigitData8: .byte $78,$CC,$CC,$78,$78,$CC,$CC,$78 ; digit 8
DigitData9: .byte $78,$8C,$0C,$7C,$CC,$CC,$CC,$78 ; digit 9
DigitDataA: .byte $00,$00,$00,$00,$00,$00,$00,$00 ; blank digit
ActivisionLogo0: .byte $00,$AD,$A9,$E9,$A9,$ED,$41,$0F
ActivisionLogo1: .byte $00,$50,$58,$5C,$56,$53,$11,$F0
ActivisionLogo2: .byte $00,$BA,$8A,$BA,$A2,$3A,$80,$FE
ActivisionLogo3: .byte $00,$E9,$AB,$AF,$AD,$E9,$00,$00
PropellerGraphicsPointerData: .byte <PropellerGraphicsData0,<PropellerGraphicsData1,<PropellerGraphicsData2
PropellerGraphicsData0: .byte $00,$04,$0A,$0A,$0C,$18,$30,$FC,$FD,$A1,$40,$11,$21,$08,$31,$65,$B0,$31,$04,$7E,$7E
PropellerGraphicsData1: .byte $00,$04,$0A,$0A,$0C,$18,$30,$FD,$FD,$A0,$41,$11,$20,$09,$B1,$64,$31,$31,$04,$7E,$7E
PropellerGraphicsData2: .byte $00,$04,$0A,$0A,$0C,$18,$30,$FD,$FC,$A1,$41,$10,$21,$09,$30,$E5,$31,$30,$04,$7E,$7E
BirdGraphicsAnimationPointerData: .byte <BirdGraphicsAnimationFrame1,<BirdGraphicsAnimationFrame1,<BirdGraphicsAnimationFrame1,<BirdGraphicsAnimationFrame0,<BirdGraphicsAnimationFrame2,<BirdGraphicsAnimationFrame2,<BirdGraphicsAnimationFrame0,<BirdGraphicsAnimationFrame1 ; $ffXX pointers to bird animation frame data
PropellerGraphicsDataStanding: .byte $00,$04,$0A,$0A,$0C,$18,$30,$FC,$FD,$A1,$41,$11,$20,$09,$31,$E5,$31,$30,$04,$7E,$7E
WindmillGraphicsPointerData: .byte <WindmillGraphicsData0,<WindmillGraphicsData1,WindmillGraphicsData2
       .byte $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA
       .byte $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA
       .byte $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA
BirdGraphicsAnimationFrame0: .byte $00,$00,$00,$40,$30,$78,$FC,$06,$04,$00,$00,$00,$00,$00,$00,$00
BirdGraphicsAnimationFrame1: .byte $00,$00,$C0,$60,$30,$78,$FC,$07,$02,$00,$00,$00,$00,$00,$00,$00
BirdGraphicsAnimationFrame2: .byte $00,$00,$00,$40,$30,$78,$FC,$36,$E4,$00,$00,$00,$00,$00,$00,$00
       .byte $80,$11,$53,$FE,$53,$11,$10,$38,$3C,$3E,$62,$20,$00,$00,$00,$00,$00
       .byte $00,$00,$00,$00
LFF45: .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$78,$F8,$F8,$F8,$F8,$F8,$A8
       .byte $80,$80,$80,$00,$00
LFF5A: .byte $82,$82,$82,$82,$82,$82,$82,$82,$82,$FE,$82,$C6,$82,$AA,$82,$92
       .byte $82
WindmillGraphicsData0: .byte $AA,$82,$C6,$82,$FE,$44,$44,$44,$6C,$44,$54,$44,$54,$54,$54,$44
WindmillGraphicsData1: .byte $54,$44,$6C,$44,$44,$44,$44,$44,$7C,$28,$28,$28,$28,$28,$28,$28
WindmillGraphicsData2: .byte $38,$28,$28,$28,$28,$28,$28,$FE
LFF93: .byte $10,$1A,$18,$28,$28,$38,$38,$48,$48,$58,$68,$68,$78,$78,$88,$88
       .byte $FE,$10,$10,$90,$91,$12,$97,$8E,$7F,$8E,$87,$02,$81,$80,$00,$00
       .byte $FE,$10,$10,$90,$11,$92,$97,$0E,$FF,$8E,$07,$82,$81,$00,$00,$00
       .byte $FE,$10,$10,$10,$91,$92,$17,$8E,$FF,$0E,$87,$82,$01,$80,$00,$00
       .byte $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA
       .byte $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA
       .byte $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$00,$F0,$00,$F0
