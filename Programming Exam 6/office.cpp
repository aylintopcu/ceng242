#include <iostream>
#include "office.h"
#include "owner.h"

using namespace std;

Office::Office(const string &property_name, int area, Owner *owner, bool having_wifi, bool having_reception)
{
    this->property_name = property_name;
    this->area = area;
    this->owner = owner;
    this->having_wifi = having_wifi;
    this->having_reception = having_reception;
    
    if (this->owner)
    {
        this->owner->add_property(this);
    }
}

float Office::valuate()
{
    if (this->having_wifi && this->having_reception)
    {
        return this->area * 5 * 1.3 * 1.5;
    }
    else if (this->having_wifi)
    {
        return this->area * 5 * 1.3;
    }
    else if (this->having_reception)
    {
        return this->area * 5 * 1.5;
    }
    else
    {
        return this->area * 5;
    }
}