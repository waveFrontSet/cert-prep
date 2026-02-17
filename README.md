# cert-prep

A terminal UI application for practicing certification exam questions. Load a
question bank from a JSON file, answer multi-select questions with immediate
color-coded feedback, and see your final score.

## Usage

```bash
cert-prep <config.json> [-n N] [-w CATEGORY:WEIGHT ...]
```

On the first run, you must provide a path to a question bank JSON file:

```bash
cert-prep ./my-questions.json
```

The config is then registered locally. On subsequent runs, you can omit the
path and pick from previously used configs via an interactive selector:

```bash
cert-prep
```

The selector lists all previously used question banks sorted by most recently used.

### Options

| Flag | Description |
| ------ | ------------- |
| `<config.json>` | Path to a question bank JSON file (required on first use) |
| `-n N`, `--sample-amount N` | Override the number of questions to sample |
| `-w CATEGORY:WEIGHT`, `--weight CATEGORY:WEIGHT` | Set category weight for stratified sampling (repeatable) |

### Examples

```bash
# First run: load a question bank
cert-prep ./aws-sa-questions.json

# Subsequent runs: select from previously used configs
cert-prep

# Sample only 10 questions
cert-prep ./aws-sa-questions.json -n 10

# Emphasize a category
cert-prep ./aws-sa-questions.json -w "AWS Storage:3" -w "AWS Security:2"
```

## Controls

| Key | Action |
| ----- | -------- |
| `Space` | Toggle answer selection |
| `Enter` | Submit answer / Next question |
| `Arrow keys` / `j` / `k` | Navigate answer choices |
| `q` / `Esc` | Quit |

Mouse clicks on checkboxes and buttons are also supported (not in all terminals).

After submitting an answer, selections are color-coded:

- **Green `[+]`** -- correct selection
- **Yellow `[O]`** -- missed correct answer
- **Red `[X]`** -- incorrect selection

## Question bank format

Question banks are JSON files with the following structure:

```json
{
  "title": "My Certification Exam",
  "sample_amount": 10,
  "category_weights": {
    "Category A": 2,
    "Category B": 1
  },
  "questions": [
    {
      "text": "Which option is correct?",
      "answer_choices": ["Option A", "Option B", "Option C"],
      "correct_answer": [1],
      "category": "Category A"
    }
  ]
}
```

| Field | Description |
| ------- | ------------- |
| `title` | Display name for the question bank |
| `sample_amount` | Number of questions to randomly sample per session |
| `category_weights` | Relative weights for stratified sampling across categories |
| `questions[].text` | The question prompt |
| `questions[].answer_choices` | List of answer options |
| `questions[].correct_answer` | Zero-indexed list of correct answer indices |
| `questions[].category` | Category label for the question |

## Building from source

Requires GHC and Cabal. Uses hpack (`package.yaml`).

```bash
cabal build
cabal run cert-prep -- ./config.json
```
