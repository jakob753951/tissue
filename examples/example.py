# TODO bug: this function breaks when input contains punctuation marks; assigned OthelloEngineer
# TODO feature: please have this accept nordic letters python
def process_text(input):
    word_count = {}

    for word in input.lower().split():
        word_count[word] = word_count.get(word, 0) + 1

    return word_count

text = 'The quick brown fox jumps over the lazy dog'
word_count = process_text(text)

print(word_count)