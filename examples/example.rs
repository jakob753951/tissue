use std::collections::HashMap;

//TODO bug: this function breaks when input contains punctuation marks; assigned OthelloEngineer
//TODO feature: please have this accept nordic letters rust
fn process_text(input: &str) -> HashMap<String, usize> {
    let mut word_count = HashMap::new();

    for word in input.split_whitespace() {
        let word = word.to_lowercase(); // Normalize the word to lowercase.
        *word_count.entry(word).or_insert(0) += 1;
    }

    word_count
}

fn main() {
    let text = "The quick brown fox jumps over the lazy dog";
    let word_count = process_text(text);

    for (word, count) in word_count.iter() {
        println!("{}: {}", word, count);
    }
}