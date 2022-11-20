#include<iostream>
#include<vector>
#include<memory>
#include<person.h>

using namespace std;

int main()
{
    const unique_ptr<vector<shared_ptr<Person>>> people = make_unique<vector<shared_ptr<Person>>>();

    people->push_back(make_shared<Person>("Ed", "F", "Mac"));
    people->push_back(make_shared<Person>("Jane", "G", "Doe"));
    people->push_back(make_shared<Person>("John", "H", "Doe"));

    for(shared_ptr<Person> p : *people){
        cout << typeid(p).name() << ": "
             << p->getFirstName() << " "
             << p->getMiddleName() << " "
             << p->getLastName() << endl;
    }

    Person p("A", "B", "C");
    Person q = p;

    cout <<  Person()
                .setFirstName("X")
                .setMiddleName("Y")
                .setLastName("Z")
                .getFirstName()
          << endl;

    return 0;
}
