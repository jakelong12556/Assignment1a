# Encryption

These notes are a collection of considerations that need to be added to our documentations as they are crucial to our application.

## Security

- We do not specify how our secure data transfer connection works. We should use **hybrid cryptography** which involves a number of steps between server and local application (refer to [this page](https://medium.com/mindorks/how-to-pass-large-data-between-server-and-client-android-securely-345fed551651))
  1. Using an asymmetric encryption (say RSA), the server generates a key pair consisting of a **public key** and a **private key**.
  2. Server saves these keys in a secure location.
  3. We take **public key** and ship it in our app (client).
  4. When we want to transfer some sensitive data to server (at runtime), we generate a passcode (aka **secret key**) using a symmetric encryption (say AES).
  5. Using this **secret key** we encrypt our large texts of data quickly.
  6. Now we use the **public key** to encrypt our **secret key**.
  7. We send this encrypted data and encrypted **secret key** combination to server (using any commonly used way to send combination of data, like JSON)
  8. Server receives this combination, extracts **encrypted data** and **encrypted secret key** from it.
  9. Server uses **private key** to decrypt the encrypted **secret key**.
  10.  Server uses decrypted **secret key** (or simply called secret key) to decrypt the encrypted data. Hence it gets the large texts of data which was sent by the client securely.
- Additionally local data of the app should probably be encrypted as this is common practice for sensitive data. (refer to [this page](https://security.stackexchange.com/questions/145742/why-encrypt-sensitive-mobile-app-data)). This is presumably especially true for a _remember password_ function as it stores email and password locally. I am unsure how this would be done best.
