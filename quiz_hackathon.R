library(shinyquiz)

messages <- create_messages('Congrats! You have successfully completed the quiz',
                            'Ahh bummer! Better luck next time',
                            'You have skipped the Quiz!')

quiz1 <- create_quiz(
  create_question(
    'What is recycled water?',
    add_choice('Rainwater collected from roofs'),
    add_choice('Water that has been used, treated, and reused', correct = TRUE),
    add_choice('Bottled spring water'),
    add_choice('Water from underground aquifers'),
    add_choice("I don't know"),
    label = 'Select answer'
  ),
  create_question(
    'What is recycled water most commonly used for in our region? (North Central Victoria)',
    add_choice('Drinking'),
    add_choice('Bathing'),
    add_choice('Irrigation and Industry', correct = TRUE),
    add_choice('Cooking'),
    add_choice("I don't know"),
    label = 'Select answer'

  ),

  options = set_quiz_options(
    progress_bar_color = '#124464',
    messages = create_messages(
      'Congrats! You have successfully completed the quiz',
      'Ahh bummer! Better luck next time',
      'You have skipped the Quiz!'
    ),
    end_on_first_wrong = FALSE
  )
)

quiz2 <- create_quiz(
  create_question(
    'Which colour pipes are used to indicate recycled water is in them?',
    add_choice('Blue'),
    add_choice('Green'),
    add_choice('Purple', correct = TRUE),
    add_choice('Red'),
    add_choice("I don't know"),
    label = 'Select answer'
  ),
  create_question(
    'What are the names for the different classes of recycled water produced by Coliban Water?',
    add_choice('Good, bad, worse'),
    add_choice('A, B, C', correct = TRUE),
    add_choice('Alpha, Beta, Omega'),
    add_choice('One, Two, Three'),
    add_choice("I don't know"),
    label = 'Select answer'
  ),
  options = set_quiz_options(
    progress_bar_color = '#124464',
    messages = create_messages(
      'Congrats! You have successfully completed the quiz',
      'Ahh bummer! Better luck next time',
      'You have skipped the Quiz!'
    ),
    end_on_first_wrong = FALSE
  )
)
quiz3 <- create_quiz(  
  create_question(
    'Can recycled water be safely used on sports grounds and public parks?',
    add_choice('Yes', correct = TRUE),
    add_choice('No'),
    add_choice('Only in winter'),
    add_choice('Only after it has rained, so it can be diluted'),
    add_choice("I don't know"),
    label = 'Select answer'
  ),
  create_question(
    'In our region, Class A recycled water is treated at...',
    add_choice('Lagoon based treatment plants'),
    add_choice('Large water reclamation plants'),
    add_choice("The 'recycled water factory' in Bendigo", correct = TRUE),
    add_choice("Coliban Water doesn't produce this class of recycled water"),
    add_choice("I don't know"),
    label = 'Select answer'
  ),
  create_question(
    'What is one connection between recycled water and the environment?',
    add_choice('Animals can drink the recycled water'),
    add_choice('The recycled water is in recycled pipes, so they are better for the environment'),
    add_choice('Recycled water can be released to waterways to support river health and habitat', correct = TRUE),
    add_choice('It does not help the environment'),
    add_choice("I don't know"),
    label = 'Select answer'
  ),
  options = set_quiz_options(
    progress_bar_color = '#124464',
    messages = create_messages(
      'Congrats! You have successfully completed the quiz',
      'Ahh bummer! Better luck next time',
      'You have skipped the Quiz!'
    ),
    end_on_first_wrong = FALSE
  )
)