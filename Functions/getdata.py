
import sys
'''!{sys.executable} -m pip install yfinance'''
import yfinance as yf
import numpy as np
import pandas as pd
from datetime import date


def getData(assets,start,end):
    i = 1
    for asset in assets:
        print(asset,'\n')
        df_assets_complete = yf.download(asset, start=start, end=end)
  
        if df_assets_complete.index[0].year == int(start[0:4]) and df_assets_complete.index[0].month == int(start[5:7]) and df_assets_complete.index[0].day == int(start[9:11]):
            if i==1:
                df_assets_close = yf.download(asset, start=start, end=end)
                # select columns
                df_assets_close = df_assets_close[['Close']]
                # rename columns
                df_assets_close = df_assets_close.rename(columns={'Close':asset})
            else:
                df_assets_close2 = yf.download(asset, start=start, end=end)
                # select columns
                df_assets_close2 = df_assets_close2[['Close']]
                # rename columns
                df_assets_close2 = df_assets_close2.rename(columns={'Close':asset})
                # mergin
                df_assets_close = df_assets_close.merge(df_assets_close2,left_index=True, right_index=True)
            i+=1
        else:
            continue

    return df_assets_close
##############################################################################################################################
