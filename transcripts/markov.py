#!/usr/bin/env python3
import sys, glob, nltk, random

def is_speech_line(line):
    # it's a speech line if there's a colon we can split on
    # and all letters to the left of the colon are uppercase
    if ':' not in line:
        return False

    left = line.split(':')[0]
    letters = [l for l in left if l.isalpha()]

    return all(l.isupper() for l in letters)

def main():
    script_dir = sys.argv[1]

    # the template script
    template = sys.argv[2]

    fnames = glob.glob(f'{script_dir}/*')

    # a map of character name -> their trigram dict, pair of words
    # mapping to list of possible next words
    char_trigrams = defaultdict(lambda: defaultdict(lambda: []))

    # title word list, too few words to use ngrams
    title_words = []

    for fname in fnames:
        lines = open(fname, 'r').readlines()
        lines = [line.rstrip() for line in lines]
        title_words += nltk.word_tokenize(lines[0])
        for line in lines[1:]:
            if not is_speech_line(line):
                continue
            # we split on the first colon, assume the character name is
            # the text before the colon and make trigrams
            splat = line.split(":")
            char_name = splat[0]
            speech = ":".join(split[1:])
            tokens = nltk.word_tokenize(speech)

            for w1, w2, w3 in nltk.trigrams(tokens, pad_left=True):
                char_trigrams[char_name][(w1, w2)].append(w3)

        # Now we have our model, we need to generate a script. It's
        # too hard to generate a convincing large scale structure for
        # a script, so we take an existing script, and just generate
        # new lines of the same length as the old lines.
        template_lines = open(template, 'r').readlines()
        template_lines = [l.rstrip() for l in template_lines]
        for line in template_lines:
            if not is_speech_line(line):
                print(line)

            splat = line.split(":")

            char_name = splat[0]

            # we want to generate the same number of words so we might
            # end up with something well-paced and dramatic
            nwords = len(nltk.word_tokenize(":".join(splat[1:])))

            # now we generate some line using the trigram model for
            # the character
            sentence = [None, None]

            for _ in range(nwords):
                last = tuple(sentence[-2:])
                next_word = random.choice(char_trigrams[char_name][last])
                sentence.append(next_word)

            sentence = " ".join(sentence[2:])

            print(f'{char_name}: {sentence}')

if __name__ == '__main__':
    main()
