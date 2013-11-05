{
    'targets': [
        {
            'target_name': 'nodeipc',
            'sources': [
                './nodeipc.cc', './ipcconn.cc'
            ],
			'cflags': ['-O3'],
			'cflags_cc': ['-O3'],
            'include_dirs': [
                "/usr/include"
            ],
            'link_settings': {
                'libraries': [
                    '/usr/lib/libqb.so'
            	]
            }
        }

    ]

}
