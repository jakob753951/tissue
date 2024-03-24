import java.util.*;

public class WordCounter {
    // TODO bug: this function breaks when input contains punctuation marks; assigned OthelloEngineer
    // TODO feature: please have this accept nordic letters java
    public static Map<String, Integer> processText(String input) {
        Map<String, Integer> wordCount = new HashMap<>();
        String[] words = input.toLowerCase().split("\\s+");

        for (String word : words) {
            wordCount.put(word, wordCount.getOrDefault(word, 0) + 1);
        }

        return wordCount;
    }

    public static void main(String[] args) {
        String text = "The quick brown fox jumps over the lazy dog";
        Map<String, Integer> wordCount = processText(text);

        for (Map.Entry<String, Integer> entry : wordCount.entrySet()) {
            System.out.println(entry.getKey() + ": " + entry.getValue());
        }
    }
}
