{
    'targets': [
        {
            'target_name': 'nodeipc',
            'sources': [
                './nodeipc.cc', './ipcconn.cc'
            ],
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
