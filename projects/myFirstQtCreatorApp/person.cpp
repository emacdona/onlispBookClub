#include "person.h"
#include<iostream>

using std::cout;
using std::endl;

Person::Person(string firstName, string middleName, string lastName)
{
    m_firstName = firstName;
    m_middleName = middleName;
    m_lastName = lastName;
}

Person::Person(const Person& other){
    cout << "lvalue Copying: " << other.toString() << endl;
    m_firstName = other.m_firstName;
    m_middleName = other.m_middleName;
    m_lastName = other.m_lastName;
}

Person::Person(const Person&& other){
    cout << "rvaule Copying: " << other.toString() << endl;
    m_firstName = other.m_firstName;
    m_middleName = other.m_middleName;
    m_lastName = other.m_lastName;
}

Person::~Person(){
    cout << this->toString()
         << " about to be deleted"
         << endl;
}

string Person::getFirstName(){
    return m_firstName;
}

string Person::getMiddleName(){
    return m_middleName;
}

string Person::getLastName(){
    return m_lastName;
}

Person& Person::setFirstName(string firstName){
    m_firstName = firstName;
    return *this;
}

Person& Person::setMiddleName(string middleName){
    m_middleName = middleName;
    return *this;
}

Person& Person::setLastName(string lastName){
    m_lastName = lastName;
    return *this;
}

std::string Person::toString() const{
    return "Person(" + m_firstName + " " + m_middleName + " " + m_lastName + ")";
}
