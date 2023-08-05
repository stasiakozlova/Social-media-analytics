import pymorphy2

morph = pymorphy2.MorphAnalyzer()

messages = open('posts_processed.txt')
normailized_messages = open('posts_processed_norm.txt', 'w')

for line in messages:
    line_norm = ' '.join(morph.parse(word)[0].normal_form for word in line.split())
    normailized_messages.write(line_norm + '\n')

messages.close()
normailized_messages.close()
