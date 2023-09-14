# coding=utf-8

def get_rank_from_type(x: str):
    if x.endswith('1d'):
        return '(:)'
    elif x.endswith('2d'):
        return '(:,:)'
    elif x.endswith('3d'):
        return '(:,:,:)'
    elif x.endswith('4d'):
        return '(:,:,:)'
    else:
        return ''

ctt = {
    'ftype' : {
        'int' : 'integer(kind=4)',
        'int4' : 'integer(kind=4)',
        'int8' : 'integer(kind=8)',
        'intA1d' : 'integer(kind=4)',
        'int8A1d' : 'integer(kind=8)',
        'intA2d' : 'integer(kind=4)',
        'intA3d' : 'integer(kind=4)',
        'float' : 'real(kind=4)',
        'real4' : 'real(kind=4)',
        'floatA1d' : 'real(kind=4)',
        'floatA2d' : 'real(kind=4)',
        'floatA3d' : 'real(kind=4)',
        'real4A1d' : 'real(kind=4)',
        'real4A2d' : 'real(kind=4)',
        'real4A3d' : 'real(kind=4)',
        'double' : 'real(kind=8)',
        'real8' : 'real(kind=8)',
        'doubleA1d' : 'real(kind=8)',
        'doubleA2d' : 'real(kind=8)',
        'doubleA3d' : 'real(kind=8)',
        'real8A1d' : 'real(kind=8)',
        'real8A2d' : 'real(kind=8)',
        'real8A3d' : 'real(kind=8)',
        'logical' : 'logical',
        'logicalA1d' : 'logical',
        'logicalA2d' : 'logical',
        'logicalA3d' : 'logical',
        'string' : 'character(*)',
        'stringA1d' : 'character(*)',
        'stringA2d' : 'character(*)',
        'stringA3d' : 'character(*)'
    }, 
    'func_get_rank' : get_rank_from_type
}