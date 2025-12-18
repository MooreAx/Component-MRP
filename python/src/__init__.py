'''
the presence of __init__.py in src makes src a package rather than just a folder
this enables relative imports within src
'''

#preloading modules using relative imports

from .inventory import Lot, Inventory 
from .demand import IntermittentDemand, AggregateDemand
from .simulation import Simulation
from .safety_stock import * #this is bad practice i think. come back to it later
from .replenishment import * #same
from .forecast_netting import *


