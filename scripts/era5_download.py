import cdsapi

years=['2017']
months=[3]
lats = [-45,-65]
lons = [157, 200]

c = cdsapi.Client()

for jj in years:
	for ii in months:

		c.retrieve(
			'reanalysis-era5-single-levels',
			{
				'product_type':'reanalysis',
				'format':'netcdf', # Supported format: grib and netcdf. Default: grib
				'grid': [0.25, 0.25],# Latitude/longitude grid: east-west (longitude) and north-south resolution (latitude). Default: 0.25 x 0.25
				'area': [lats[0], lons[0], lats[1], lons[1]],# North, West, South, East. Default: global
				'variable':[
					'10m_u_component_of_wind','10m_v_component_of_wind','10m_wind_speed',
					'total_precipitation'
				],
				'year':jj,
				'month':[
					str(ii)
				],
				'day':[
					'1','2','3',
					'4','5','6',
					'7','8','9',
					'10','11','12',
					'13','14','15',
					'16','17','18',
					'19','20','21',
					'22','23','24',
					'25','26','27',
					'28','29','30',
					'31'
				],
				'time':[
					'00:00','01:00','02:00',
					'03:00','04:00','05:00',
					'06:00','07:00','08:00',
					'09:00','10:00','11:00',
					'12:00','13:00','14:00',
					'15:00','16:00','17:00',
					'18:00','19:00','20:00',
					'21:00','22:00','23:00'
				]
			},
			'era5_'+jj+'_'+'{:02d}'.format(ii)+'_N'+'{:02d}'.format(lats[1])+'_W'+'{:03d}'.format(lons[0])+'_S'+'{:02d}'.format(lats[0])+'_E'+'{:03d}'.format(lons[1])+'.nc')
