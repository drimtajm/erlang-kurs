-module(rrclib).

-compile(export_all).

-include("../include/EUTRA-RRC-Definitions.hrl").

make_rrc_connection_request(ID) ->
#'RRC-UL-CCCH-Message'{
   message = 
       {c1,{rrcConnectionRequest,
	    #'RRC-RRCConnectionRequest'{
	       criticalExtensions = 
		   {'rrcConnectionRequest-r8',
		    #'RRC-RRCConnectionRequest-r8-IEs'{
		       'ue-Identity' = 
			   {randomValue,{0,<<255,255,229,156,ID>>}},
		       establishmentCause = 'mo-Signalling',
		       spare = {7,<<0>>}}}}}}}.

make_rrc_connection_setup(ID) ->
    #'RRC-DL-CCCH-Message'{
       message = 
	   {c1,
	    {rrcConnectionSetup,
	     #'RRC-RRCConnectionSetup'{
		'rrc-TransactionIdentifier' = ID,
		criticalExtensions = 
		    {c1,
		     {'rrcConnectionSetup-r8',
		      #'RRC-RRCConnectionSetup-r8-IEs'{
			 radioResourceConfigDedicated = 
			     #'RRC-RadioResourceConfigDedicated'{
				'srb-ToAddModList' = 
				    [#'RRC-SRB-ToAddMod'{
					'srb-Identity' = 1,
					'rlc-Config' = 
					    {explicitValue,
					     {am,
					      #'RRC-RLC-Config_am'{
						 'ul-AM-RLC' = 
						     #'RRC-UL-AM-RLC'{
							't-PollRetransmit' = ms5,pollPDU = p4,pollByte = kB25,
							maxRetxThreshold = t1},
						 'dl-AM-RLC' = 
						     #'RRC-DL-AM-RLC'{
							't-Reordering' = ms0,'t-StatusProhibit' = ms0}}}},
					logicalChannelConfig = 
					    {explicitValue,
					     #'RRC-LogicalChannelConfig'{
						'ul-SpecificParameters' = 
						    #'RRC-LogicalChannelConfig_ul-SpecificParameters'{
						       priority = 1,prioritisedBitRate = kBps64,
						       bucketSizeDuration = ms50,logicalChannelGroup = 0}}}}],
				'mac-MainConfig' = 
				    {explicitValue,
				     #'RRC-MAC-MainConfig'{
					'ul-SCH-Config' = 
					    #'RRC-MAC-MainConfig_ul-SCH-Config'{
					       'maxHARQ-Tx' = n4,'periodicBSR-Timer' = sf10,
					       'retxBSR-Timer' = sf10240,ttiBundling = false},
					timeAlignmentTimerDedicated = sf500,
					'phr-Config' = 
					    {setup,
					     #'RRC-MAC-MainConfig_phr-Config_setup'{
						'periodicPHR-Timer' = sf200,'prohibitPHR-Timer' = sf200,
						'dl-PathlossChange' = dB3}}}},
				physicalConfigDedicated = 
				    #'RRC-PhysicalConfigDedicated'{
				       'pdsch-ConfigDedicated' = 
					   #'RRC-PDSCH-ConfigDedicated'{'p-a' = dB0},
				       'pusch-ConfigDedicated' = 
					   #'RRC-PUSCH-ConfigDedicated'{
					      'betaOffset-ACK-Index' = 10,'betaOffset-RI-Index' = 9,
					      'betaOffset-CQI-Index' = 10}}}}}}}}}}.
