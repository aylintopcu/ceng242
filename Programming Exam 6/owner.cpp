#include <iostream>
#include <string>
#include <vector>
#include "owner.h"

using namespace std;

Owner::Owner()
{
}

Owner::Owner(const string &name, float balance)
{
    this->name = name;
    this->balance = balance;
}

void Owner::print_info()
{
}

string &Owner::get_name()
{
    return this->name;
}

void Owner::add_property(Property *property)
{
    this->properties.push_back(property);
}

void Owner::buy(Property *property, Owner *seller)
{
    float property_value = property->valuate();
    
    cout << "[BUY] Property: " << property->get_name() << " Value: " << property_value << "$ " << seller->get_name() << "--->" << this->name << endl;
    
    if (this->balance >= property_value)
    {
        for (int i = 0; i < seller->properties.size(); i++)
        {
            if (seller->properties[i] == property)
            {
                this->add_property(property);
                this->balance -= property_value;
                seller->balance += property_value;
                seller->properties.erase(seller->properties.begin() + i);
                property->set_owner(this);
                return;
            }
        }
        
        cout << "[ERROR] Transaction  on  unowned  property" << endl;
    }
    else
    {
        cout << "[ERROR] Unaffordable  property" << endl;
    }
}

void Owner::sell(Property *property, Owner *owner)
{
    float property_value = property->valuate();
    
    cout << "[SELL] Property: " << property->get_name() << " Value: " << property_value << "$ " << this->name << "--->" << owner->get_name() << endl;
    
    if (owner->balance >= property_value)
    {
        for (int i = 0; i < this->properties.size(); i++)
        {
            if (this->properties[i] == property)
            {
                owner->add_property(property);
                owner->balance -= property_value;
                this->balance += property_value;
                this->properties.erase(this->properties.begin() + i);
                property->set_owner(owner);
                return;
            }
        }
        
        cout << "[ERROR] Transaction  on  unowned  property" << endl;
    }
    else
    {
        cout << "[ERROR] Unaffordable  property" << endl;
    }
}

void Owner::list_properties()
{
    cout << "Properties of " << this->name << ":" << endl;
    cout << "Balance: " << this->balance << "$" << endl;
    
    for (int i = 0; i < this->properties.size(); i++)
    {
        cout << i + 1 << ". " << properties[i]->get_name() << endl;
    }
}