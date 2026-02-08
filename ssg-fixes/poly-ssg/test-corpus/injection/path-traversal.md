---
title: Path Traversal Attempts
date: 2024-01-01
template: ../../../etc/passwd
---

# Path Traversal Tests

![Image](../../../etc/passwd)
![Image](....//....//etc/passwd)
![Image](..%2f..%2f..%2fetc%2fpasswd)
![Image](..%252f..%252f..%252fetc%252fpasswd)

[Link](file:///etc/passwd)
[Link](file://localhost/etc/passwd)

![Image](/etc/passwd)
![Image](C:\Windows\System32\config\SAM)
![Image](\\server\share\file)

## Null Bytes

![Image](../../../etc/passwd%00.png)
[Link](../../../etc/passwd%00.html)

## URL Encoding Tricks

![Image](%2e%2e%2f%2e%2e%2f%2e%2e%2fetc%2fpasswd)
![Image](..%c0%af..%c0%af..%c0%afetc%c0%afpasswd)
![Image](..%25c0%25af..%25c0%25af..%25c0%25afetc%25c0%25afpasswd)
