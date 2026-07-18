# cert-prep

A terminal UI application for practicing certification exam questions. Load a
question bank from a JSON file, answer multi-select questions with immediate
color-coded feedback, and see your final score.

## Installation

### Linux / macOS

```bash
curl -fsSL https://raw.githubusercontent.com/waveFrontSet/cert-prep/main/install.sh | sh
```

This installs the latest release to `~/.local/bin`. To choose a different
directory or pin a version:

```bash
curl -fsSL https://raw.githubusercontent.com/waveFrontSet/cert-prep/main/install.sh | sh -s -- -b /usr/local/bin v0.5.0
```

Note: the prebuilt macOS binary is Apple Silicon (aarch64) only. On Intel
Macs, [build from source](#building-from-source).

### Windows

```powershell
powershell -ExecutionPolicy Bypass -c "irm https://raw.githubusercontent.com/waveFrontSet/cert-prep/main/install.ps1 | iex"
```

This installs `cert-prep.exe` to `%LOCALAPPDATA%\Programs\cert-prep` (override
with the `CERT_PREP_INSTALL_DIR` environment variable) and prints a hint if
that directory is not on your `PATH`.

### Manual download

Grab the archive for your platform from the
[releases page](https://github.com/waveFrontSet/cert-prep/releases), extract
it, and put the `cert-prep` binary somewhere on your `PATH`.

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
| `--version` | Print the version and exit |

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
| `h` / `l` | When reviewing, travel to previous / next question |
| `a` | When reviewing, request an AI explanation (see below) |
| `Ctrl-f` / `Ctrl-b` | Scroll the explanation page-wise down / up |
| `q` / `Esc` | Quit |

Mouse clicks on checkboxes and buttons are also supported (not in all terminals).

After submitting an answer, selections are color-coded:

- **Green `[+]`** -- correct selection
- **Yellow `[O]`** -- missed correct answer
- **Red `[X]`** -- incorrect selection

## AI explanations (optional)

With an API key, pressing `a` while reviewing an answer streams an AI-generated
explanation of the correct answer (and why the incorrect choices are wrong)
into the TUI:

```bash
export GEMINI_API_KEY=your-key
cert-prep
```

Without the environment variable, the feature is simply disabled.

By default, explanations use the `gemini-2.5-flash` model via Gemini's
OpenAI-compatible endpoint. Model, endpoint, and system prompt can be
overridden in `settings.json` (see [Configuration files](#configuration-files)):

```json
{
  "aiModel": "gemini-2.5-flash",
  "aiBaseUrl": "https://generativelanguage.googleapis.com/v1beta/openai",
  "aiSystemPrompt": "You are a friendly, concise mentor ..."
}
```

All keys are optional; any OpenAI-compatible endpoint should work.

## Trophies

You can earn trophies for feats such as answer streaks, fast answers, or a
flawless session. Trophies are tracked per question bank and shown in the TUI
when earned.

## Configuration files

State lives in the XDG config directory (`~/.config/cert-prep/` on
Linux/macOS, `%APPDATA%\cert-prep\` on Windows):

| File | Purpose |
| ------ | --------- |
| `registry.json` | Previously used question banks (feeds the config selector) |
| `settings.json` | Optional AI explanation settings |
| `trophies/` | Earned trophies, one file per question bank |

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
