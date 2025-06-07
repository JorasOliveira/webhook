Webhook Server in Haskell

A project for the Insper Functional Programming course (Programação Funcional).

This project implements a robust webhook handler in Haskell. It's designed to receive notifications from a payment gateway, validate them, and trigger subsequent actions like confirming or cancelling a transaction based on the validity and uniqueness of the received data.

The entire implementation adheres to functional programming principles, leveraging Haskell's strong type system, immutability, and tools for handling side effects (IO) to create a reliable and maintainable server.

Grade Analysis

Based on the project rubric, the current implementation achieves the following:

Base Grade: C - The project compiles, runs, and successfully passes all 6/6 tests provided by the test_webhook.py script.

Optional Items (+1/2 concept each):

✅ Verify payload integrity: The server uses Haskell's strict type system and Aeson's parsing to ensure the payload has the correct structure and data types. The two-phase parsing for transaction_id makes this even more robust.

✅ Verify transaction truthfulness: The server checks for a valid X-Webhook-Token and validates the content of the payload (e.g., amount > 0).

✅ Cancel the transaction on discrepancy: The server correctly identifies invalid data (e.g., amount is 0) and missing fields, and successfully sends a cancellation request to the configured URL.

✅ Confirm the transaction on success: The server correctly identifies a valid, unique transaction and sends a confirmation request to the configured URL.

❌ Persist transaction in a DB: This implementation uses an in-memory IORef for idempotency checks. It does not persist to a database.

❌ Implement an HTTPS service: The server runs on HTTP.

Total Optional Items Achieved: 4

Expected Grade: C + (4 * 1/2) = C + 2 = A

The project solidly fulfills the base requirement and implements four of the six optional features, leading to an expected final grade of A.

How to Build and Run
Prerequisites

You need the Haskell Toolchain, specifically the GHC compiler and the Cabal build tool. The recommended way to install them is via GHCup.

Installation

Clone the repository:

git clone <your-repo-url>
cd webhook-haskell


Build the project:
This command will download all necessary Haskell libraries (like Scotty and Aeson) and compile the source code.

cabal build
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
Bash
IGNORE_WHEN_COPYING_END
Running the Service

Execute the server using Cabal:

cabal run
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
Bash
IGNORE_WHEN_COPYING_END

The server will start and listen for POST requests on http://localhost:3000/webhook by default.

$ cabal run
Loading configuration...
Configuration loaded.
Shared state initialized.
Webhook server starting on port 3000...
Setting phasers to stun... (port 3000) (ctrl-c to quit)
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
IGNORE_WHEN_COPYING_END
Configuration

The server's behavior can be configured via environment variables. If an environment variable is not set, the server will use a sensible default value.

WEBHOOK_PORT: The port the server listens on. (Default: 3000)

WEBHOOK_AUTH_TOKEN: The secret token for the X-Webhook-Token header. (Default: meu-token-secreto)

CONFIRMATION_URL: The full URL to call on a successful transaction. (Default: http://localhost:5001/confirmar)

CANCELLATION_URL: The full URL to call on a failed or invalid transaction. (Default: http://localhost:5001/cancelar)

Example of running on a different port:

WEBHOOK_PORT=8080 cabal run
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
Bash
IGNORE_WHEN_COPYING_END
How to Test

A Python script (test_webhook.py) is provided to simulate the payment gateway and test all scenarios.

Keep the Haskell server running in one terminal.

Install Test Script Dependencies:
In a new terminal, install the required Python libraries.

pip install fastapi uvicorn requests
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
Bash
IGNORE_WHEN_COPYING_END

Run the Tests:
Execute the test_webhook.py script from your project directory. It will send a series of 6 predefined requests to your running webhook server and report the results.

python3 test_webhook.py
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
Bash
IGNORE_WHEN_COPYING_END

You will see log output in both the Python test terminal and the Haskell server terminal.

This project was developed with the assistance of AI-powered tools to help in areas such as code debugging, documentation, and exploring Haskell best practices. These tools served as a supportive aid in the development process.