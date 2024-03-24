-- TODO bug: this function breaks when input contains punctuation marks; assigned OthelloEngineer
-- TODO feature: please have this accept nordic letters lua
function process_text(input)
    local word_count = {}
    for word in string.gmatch(input:lower(), "%w+") do
        if word_count[word] then
            word_count[word] = word_count[word] + 1
        else
            word_count[word] = 1
        end
    end
    return word_count
end

local text = "The quick brown fox jumps over the lazy dog"
local word_count = process_text(text)

for word, count in pairs(word_count) do
    print(word, count)
    print(" ")
end