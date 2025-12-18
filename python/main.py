#simulation file
#import classes
from src import *
import pandas as pd

sim_length = 1000 #weeks

#define demand
ON = IntermittentDemand(customer = "ON", rate = 0.3, mean = 1000, stdev = 100, fresh=12, fcbias = 0) #12 weeks fresh = 90 days
SK = IntermittentDemand(customer = "SK", rate = 1, mean = 50, stdev = 10, fresh=50, fcbias = 0) #12 weeks fresh = 90 days

#append to list
IntDemand_list = []
IntDemand_list.append(ON)
IntDemand_list.append(SK)

#generate demand for 1 year
ON50 = ON.generate(sim_length, seed=3)
SK50 = SK.generate(sim_length, seed=4)

AD = AggregateDemand()
AD.add(ON50)
AD.add(SK50)


ss = get_safetystock_1(IntDemand_list, leadtime=4, service_level=0.95)

print("\n running ss2")
ss2 = get_safetystock_2(IntDemand_list, leadtime=4, service_level=0.95)

#i think we need to do safety stock per freshness requirement, not just total demand,
#and then reorder if onhand is less than ss for any of the components.


#this aggregates demand and converts to data frame, with column names = fresh requirement
print("Aggregate demand:")
print(pd.DataFrame(AD.aggregate_demand))

print("\nAggregate forecast:")
print(pd.DataFrame(AD.aggregate_forecast))

sim_log = {
    "WOH_target": [],
    "EO_fraction": [],
    "service_level": [],
    "fill_rate": []
}

#monte carlo simulation
wks_on_hand_target = 0
while wks_on_hand_target <= 15:


    # Initialize sales_log as a dictionary of lists (columns)
    sales_log = {
        "date": [],
        "customer": [],
        "ordered": [],
        "filled": [],
        "short": []
    }

    #create sim instance for date tracking
    sim = Simulation()

    #set starting inventory:
    Inv = Inventory(sim)
    Inv.replenish(qty=5000, thc=0.3, leadtime=0)

    #simulate

    horizon = len(AD.components[0]["demand"])

    for i in range(horizon):
        sim.advance_time()

        #run forecast netting
        NettingResult = forecast_netting(AD, Inv)
        fcn_sales_log = NettingResult["sales_log"]
        fcn_short = NettingResult["short"]
        fcn_ending_inv= NettingResult["ending_inv"]
        

        #choose replenishment method
        replen = 3
        if replen == 1:
            if replenish_1(fcn_ending_inv, AD, ss):
                Inv.replenish(qty=1000, thc=0.3, leadtime=4) 
        elif replen == 2:
            if replenish_2(fcn_ending_inv, AD, ss2):
                Inv.replenish(qty=5000, thc=0.3, leadtime=4)
        elif replen == 3:
            ReplenResult = replenish_0(fcn_ending_inv, AD, WOH=wks_on_hand_target)
            reorder = ReplenResult["reorder"]
            qty = ReplenResult["qty"]
            if reorder:
                min_qty = 1000
                reorderqty = max(qty, min_qty)
                Inv.replenish(qty=reorderqty, thc=0.3, leadtime=4)


        for dcomp in AD.components:
            f = dcomp["fresh"]
            dmd = dcomp["demand"]
            cust = dcomp["customer"]
                        
            starting = Inv.qtyavailable(f)
            short = Inv.sell_fifo(demand=dmd[i], fresh=f)
            ending = Inv.qtyavailable(f)
            d = dmd[i]
            filled = starting - ending

            #log sale
            sales_log["date"].append(sim.date)
            sales_log["customer"].append(cust)
            sales_log["ordered"].append(d)
            sales_log["filled"].append(filled)
            sales_log["short"].append(short)
            

            print(f"day = {sim.date:>4}, customer = {cust}, fresh = {f:>2}, demand = {d:>5}, starting = {starting:>5}, filled = {filled:>5}, ending = {ending:>5}, short = {short:>5}")
        

    # Convert the dictionary to a pandas DataFrame
    df_sales = pd.DataFrame(sales_log)

    # Print the resulting DataFrame
    print(df_sales)

    eofrac = Inv.print_inventory()

    total_demand = sum([sum(dcomp["demand"]) for dcomp in AD.components])
    print(f"Total demand = {total_demand}")

    total_sales = sum(sales_log['filled'])
    print(f"Total sales = {total_sales}")

    total_short = sum(sales_log['short'])
    print(f"Total short = {total_short}")

    stockouts = (df_sales["short"] > 0).sum()
    orders = (df_sales["ordered"] > 0).sum()
    service_level = 1 - stockouts / orders
    print(f"Service level =  {service_level}")

    fill_rate = round((sum(sales_log['filled'])/total_demand), 3)
    print(f"Fill rate = {fill_rate}")


    #write data to sim log
    sim_log["WOH_target"].append(wks_on_hand_target)
    sim_log["EO_fraction"].append(eofrac)
    sim_log["service_level"].append(service_level)
    sim_log["fill_rate"].append(fill_rate)

    wks_on_hand_target += 1

df_simlog = pd.DataFrame(sim_log)

df_simlog.to_csv("sampleoutput.csv", index = False)