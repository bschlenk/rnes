abs .... absolute OPC $LLHH	 	operand is address $HHLL \*
abs,X .... absolute, X-indexed OPC $LLHH,X	 	operand is address; effective address is address incremented by X with carry **
abs,Y		....	absolute, Y-indexed	 	OPC $LLHH,Y operand is address; effective address is address incremented by Y with carry \*\*

# .... immediate OPC #\$BB operand is byte BB

impl .... implied OPC operand implied
ind .... indirect OPC ($LLHH)	 	operand is address; effective address is contents of word at address: C.w($HHLL)
X,ind .... X-indexed, indirect OPC ($LL,X)	 	operand is zeropage address; effective address is word in (LL + X, LL + X + 1), inc. without carry: C.w($00LL + X)
ind,Y .... indirect, Y-indexed OPC ($LL),Y	 	operand is zeropage address; effective address is word in (LL, LL + 1) incremented by Y with carry: C.w($00LL) + Y
rel .... relative OPC $BB	 	branch target is PC + signed offset BB ***
zpg		....	zeropage	 	OPC $LL operand is zeropage address (hi-byte is zero, address = $00LL)
zpg,X		....	zeropage, X-indexed	 	OPC $LL,X operand is zeropage address; effective address is address incremented by X without carry **
zpg,Y .... zeropage, Y-indexed OPC \$LL,Y operand is zeropage address;
effective address is address incremented by Y without carry **

000 (zero page,X)
001 zero page
010 #immediate
011 absolute
100 (zero page),Y
101 zero page,X
110 absolute,Y
111 absolute,X
