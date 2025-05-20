# webhook
project para eletiva de programacao funcional

# Project Webhook - Functional Programming

## Project Description (High Level)

This project involves creating a webhook to integrate a payment gateway into a web commerce system. [cite: 3, 4] The webhook will receive HTTP POST requests from the payment gateway when a payment event occurs (e.g., payment confirmation). [cite: 4, 6] It needs to process these notifications, validate the payment information, and take appropriate actions based on the data received. [cite: 18, 19] The communication is asynchronous, and the webhook must be prepared to handle these requests. [cite: 8]

## Requirements

* Create an HTTP (or HTTPS) service that exposes a POST route for receiving payment notifications. [cite: 18, 21]
* The service must parse and validate the JSON payload of the incoming request. [cite: 18] The expected payload includes fields like "event", "transaction\_id", "amount", "currency", and "timestamp". [cite: 18]
* Verify if the payment is genuinely correct. [cite: 19]
* Ensure the uniqueness of the payment. [cite: 19]
* The project must pass the minimum tests provided (`test_webhook.py`). [cite: 21, 23]
* The project is individual. [cite: 21]
* It can be developed in any functional programming language. [cite: 21]
* A comprehensive `README.md` file is required, detailing the project and instructions on how to install and run it. [cite: 21]
* **Delivery Date:** June 10, 2025, at 23:59 via GitHub. [cite: 21]

### Operational Details:
* **Successful Transaction:** If a transaction is okay, return a 200 status code and make a request to a confirmation URL. [cite: 20]
* **Incorrect Transaction Data:** If information like the amount is wrong, cancel the transaction by making a request. [cite: 20]
* **Missing Information (except transaction\_id):** If any required information (other than `transaction_id`) is missing, cancel the transaction by making a request. [cite: 20]
* **Invalid Token:** If the token is incorrect, treat the transaction as false and ignore it. [cite: 20]
* **General Incorrect Transaction:** If a transaction is not okay (and not covered by the above), do not return a 400 error. (The specific action for this case beyond not returning 400 is not detailed, but it implies it shouldn't be treated as a client error directly back to the gateway). [cite: 20]

## Extra Requirements (Optional Items for +1/2 grade each) [cite: 22]

* The service should verify the integrity of the payload. [cite: 22]
* The service should implement a mechanism to verify the truthfulness of the transaction. [cite: 22]
* The service should cancel the transaction in case of discrepancies. [cite: 22]
* The service should confirm the transaction in case of success. [cite: 22]
* The service should persist the transaction in a database. [cite: 22]
* Implement an HTTPS service. [cite: 22]

## Rubric [cite: 22]

* **I:** No delivery or irrelevant submission. [cite: 22]
* **D:** Project is incomplete. [cite: 22]
* **C:** Project passed the minimum test. [cite: 22]
* Each optional item successfully implemented adds +1/2 to the concept. [cite: 22]
* **Late Delivery:** 1 concept deduction. [cite: 22]

## How to Run the Project

1.  **Prerequisites:** Ensure you have the necessary environment for your chosen functional programming language.
2.  **Installation:**
    * Clone the repository.
    * Install any project-specific dependencies (refer to the language/framework specific instructions you'll add here).
3.  **Running the Service:**
    * Execute the main file to start the HTTP server. (Provide the specific command here, e.g., `python main.py`, `sbt run`, etc.)
    * The server should now be listening for POST requests on the configured webhook URL.

### Testing the Project

A Python script (`test_webhook.py`) is provided for testing. [cite: 23]

1.  **Install Test Script Dependencies:**
    ```bash
    pip install fastapi uvicorn requests
    ```
    [cite: 25]
2.  **Run the Tests:**
    Execute the `test_webhook.py` script from your terminal. It will send requests to your running webhook service.
    The script uses a default payload:
    ```json
    {
        "event": "payment_success",
        "transaction_id": "abc123",
        "amount": 49.90,
        "currency": "BRL",
        "timestamp": "2025-05-11T16:00:00Z"
    }
    ```
    [cite: 24]
    It is also possible to pass other values during execution. [cite: 24]
    *Note: The test script does not verify database persistence.* [cite: 24]

---

*This project was developed with the assistance of AI-powered tools to help in areas such as code debugging, and documentation, and best practices. These tools served as a supportive aid in the development process.*