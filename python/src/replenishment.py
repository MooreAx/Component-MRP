#add imports here
from .inventory import Inventory
from .demand import AggregateDemand

def replenish_0(Inv, AD, WOH):
    #models current replenishment strategy - replenish if LCD fresh inventory < X weeks on hand
    LCD = AD.LCD_fresh
    InvLCD = Inv.qtyavailable(max_age = LCD)
    MeanDemand = AD.mean_total_demand

    reorder = False
    reorder_qty = 0
    if InvLCD / MeanDemand < WOH:
        reorder = True
        reorder_qty = 6 * MeanDemand - LCD
    
    print(f"mean demand = {MeanDemand}, required inv = {WOH} x {MeanDemand} = {WOH * MeanDemand}. LCD units = {InvLCD}. Reorder = {reorder}")

    return {"reorder": reorder, "qty": reorder_qty}

def replenish_1(Inv, AD, SS):
    #replenish if total LCD fresh inventory is less than aggregate demand safety stock

    #get lowest common denominator for freshness requirements
    LCD = AD.LCD_fresh
    
    #get inventory that meets the LCD req't
    InvLCD = Inv.qtyavailable(max_age=LCD)

    return InvLCD < SS #return True if we need to replenish, False if we don't

def replenish_2(Inv, AD, SS):
    #replenish if inventory per freshness requrement is less than safety stock for that freshness requirement
    
    reorder = False

    for fresh, ss in SS.items():
        freshqty = Inv.qtyavailable(max_age=fresh)
        if freshqty < ss:
            reorder = True

    #could add logic in here to figure out the largest "deficit" and reorder a qty equal to that amt

    return reorder
