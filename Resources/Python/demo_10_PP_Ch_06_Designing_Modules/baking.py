# -*- coding: utf-8 -*-
"""
Created on Mon Feb 15 10:35:58 2021

@author: le279259
"""

import temperature_program

def get_preheating_instructions(fahrenheit: float) -> str:
    """Return instructions for preheating the oven in fahreneheit degrees and
    Celsius degrees.

    get_preheating_instructions(500)
    'Preheat oven to 500 degrees F (260.0 degrees C).'
    """

    cels = str(temperature_program.convert_to_celsius(fahrenheit))
    fahr = str(fahrenheit)
    return 'Preheat oven to ' + fahr + ' degrees F ('+ cels +' degrees C).'


fahr = float(input('Enter the baking temperature in degrees Fahrenheit: '))
print(get_preheating_instructions(fahr))

