use crate::{compile, Emulator, Sentence, Token};

#[test]
fn test_pwn() {
    let mut emulator = Emulator::from_seed(1069);
    let mut cnt = 0;
    let mut add_sentence = |tokens: &[Token]| {
        emulator.insert_instructions(cnt, compile(&Sentence::try_from(tokens).unwrap()));
        cnt += 1;
    };

    use Token::*;
    add_sentence(&[Const(39), Is, Push]);
    add_sentence(&[Register(1), Become, Const(0)]);
    add_sentence(&[Register(1), And, Register(1), Read, Register(1), And, Register(1)]);
    add_sentence(&[Register(1), Is, Win]);

    for _ in 0..1000 {
        let score = emulator.execute_all();
        assert_eq!(score, 1);
    }
}
