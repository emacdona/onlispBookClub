#ifndef PERSON_H
#define PERSON_H

#include<string>

using std::string;

class Person {
public:
    Person() = default;
    Person(string, string, string);
    Person(const Person&);
    Person(const Person&&);
    ~Person();
    string getFirstName();
    string getMiddleName();
    string getLastName();
    Person& setFirstName(string);
    Person& setMiddleName(string);
    Person& setLastName(string);
    string toString() const;

private:
    string m_firstName;
    string m_middleName;
    string m_lastName;
};

#endif // PERSON_H
