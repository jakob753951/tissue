# TODO bug: this function breaks when input contains punctuation marks; assigned OthelloEngineer
# TODO feature: please have this accept nordic letters ruby
def process_text(input)
  word_count = Hash.new(0)

  input.downcase.split.each do |word|
    word_count[word] += 1
  end

  word_count
end

text = 'The quick brown fox jumps over the lazy dog'
word_count = process_text(text)

puts word_count
