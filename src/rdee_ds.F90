Module rdee_ds
    ! This module contains implement of generic linked list in Fortran
    Use rdee_base
    Use rdee_algo
    Use rdee_um

    ! Private

    ! public :: node, list
    ! public :: operator(==)

    Type node
    ! this class implement the node for list and dict structure, via generic data type in scalar and 1-d array
    ! To be simple, the pointers within type should never point to target variables!
    ! always allocate them, i.e., use the noname variable, so it can be deallocated!
        Type(node),Pointer :: next => null(), prev => null()
        class(*), Allocatable :: key
        Class(*), Allocatable :: item, item1d(:) ! , item2d(:, :) => null(), item3d(:, :, :) => null()
        integer :: style = 0  ! 1 - v node, 2 - v1d node, -1 - k-v node, -2 - k-v1d node
      contains
        final :: node_destructor
        procedure :: reset => node_reset

        Procedure :: print => node_print
      
        procedure ::node_get_item
        Procedure :: node_get_item1d_int4
        Procedure :: fget_int4 => node_fget_item_int4
        Procedure :: fget1d_int4 => node_fget_item1d_int4
        Procedure :: node_get_item1d_int8
        Procedure :: fget_int8 => node_fget_item_int8
        Procedure :: fget1d_int8 => node_fget_item1d_int8
        Procedure :: node_get_item1d_real4
        Procedure :: fget_real4 => node_fget_item_real4
        Procedure :: fget1d_real4 => node_fget_item1d_real4
        Procedure :: node_get_item1d_real8
        Procedure :: fget_real8 => node_fget_item_real8
        Procedure :: fget1d_real8 => node_fget_item1d_real8
        Procedure :: node_get_item1d_logical
        Procedure :: fget_logical => node_fget_item_logical
        Procedure :: fget1d_logical => node_fget_item1d_logical
        Procedure :: node_get_item1d_string
        Procedure :: fget_string => node_fget_item_string
        Procedure :: fget1d_string => node_fget_item1d_string
        Generic :: get => node_get_item, &
            node_get_item1d_int4, &
            node_get_item1d_int8, &
            node_get_item1d_real4, &
            node_get_item1d_real8, &
            node_get_item1d_logical, &
            node_get_item1d_string
        Procedure :: node_set_v, node_set_v1d, node_set_k_v, node_set_k_v1d
        Generic :: set => node_set_v, node_set_v1d, node_set_k_v, node_set_k_v1d

        Procedure :: getKey => node_getKey

        Procedure :: hasKey => node_hasKey
    End Type

    Type list
        Type(node),pointer :: head => null(), tail => null()
        integer :: size = 0
        integer :: style = 0
      Contains
        final :: list_destructor
        Procedure :: print => list_print
        Procedure :: reset => list_reset

        Procedure :: list_get_node
        Procedure :: list_get_int
        Procedure :: list_get_int8
        Procedure :: list_get_intA1d
        Procedure :: list_get_int8A1d
        Procedure :: list_get_real4
        Procedure :: list_get_real4A1d
        Procedure :: list_get_real8
        Procedure :: list_get_real8A1d
        Procedure :: list_get_logical
        Procedure :: list_get_logicalA1d
        Procedure :: list_get_string
        Procedure :: list_get_stringA1d
        Generic :: get => &
            list_get_node,&
            list_get_int,&
            list_get_int8,&
            list_get_intA1d,&
            list_get_int8A1d,&
            list_get_real4,&
            list_get_real4A1d,&
            list_get_real8,&
            list_get_real8A1d,&
            list_get_logical,&
            list_get_logicalA1d,&
            list_get_string,&
            list_get_stringA1d
        Procedure :: fget_node => list_fget_node
        Procedure :: fget_int => list_fget_int
        Procedure :: fget_int8 => list_fget_int8
        Procedure :: fget_real4 => list_fget_real4
        Procedure :: fget_real8 => list_fget_real8
        Procedure :: fget_logical => list_fget_logical
        Procedure :: fget_string => list_fget_string
        Procedure :: fget1d_int => list_fget_intA1d
        Procedure :: fget1d_int8 => list_fget_int8A1d
        Procedure :: fget1d_real4 => list_fget_real4A1d
        Procedure :: fget1d_real8 => list_fget_real8A1d
        Procedure :: fget1d_logical => list_fget_logicalA1d
        Procedure :: fget1d_string => list_fget_stringA1d
        Procedure :: list_append, list_append_a1d
        Generic :: append => list_append, list_append_a1d
        Procedure :: list_insert, list_insert_a1d
        Generic :: insert => list_insert, list_insert_a1d
        Procedure :: list_set, list_set_a1d
        Generic :: set => list_set, list_set_a1d

        Procedure :: list_delete_by_value, list_delete_by_value_a1d
        Generic :: del => list_delete_by_value, list_delete_by_value_a1d
        Procedure :: pop => list_delete_by_pos
        Procedure :: popLast => list_delete_last
        Procedure :: popFirst => list_delete_first
        Procedure :: popKey => list_delete_by_key

        Procedure :: list_index, list_index_a1d
        Generic :: index => list_index, list_index_a1d
        Procedure :: has => list_has

        Procedure :: keys => list_keys
        Procedure :: extend => list_extend

        Procedure :: list_toArray_int4
        Procedure :: list_toArray_int8
        Procedure :: list_toArray_real4
        Procedure :: list_toArray_real8
        Procedure :: list_toArray_logical
        Procedure :: list_toArray_string
        Generic :: toArray => &
            list_toArray_int4,&
            list_toArray_int8,&
            list_toArray_real4,&
            list_toArray_real8,&
            list_toArray_logical,&
            list_toArray_string
        Procedure :: reverse => list_reverse

    End Type

    Type dict
        type(list),allocatable :: lists(:)
        integer :: size = 0
        procedure(hash_a2i), pointer, nopass :: hashFunc
      Contains
        final :: dict_destructor
        Procedure :: dict_get
        Procedure :: dict_get1d_int4
        Procedure :: dict_get1d_int8
        Procedure :: dict_get1d_real4
        Procedure :: dict_get1d_real8
        Procedure :: dict_get1d_logical
        Procedure :: dict_get1d_string
        Generic :: get => dict_get, &
            dict_get1d_int4,&
            dict_get1d_int8,&
            dict_get1d_real4,&
            dict_get1d_real8,&
            dict_get1d_logical,&
            dict_get1d_string
        Procedure :: fget1d_int4 => dict_fget1d_int4
        Procedure :: fget_int4 => dict_fget_int4
        Procedure :: fget1d_int8 => dict_fget1d_int8
        Procedure :: fget_int8 => dict_fget_int8
        Procedure :: fget1d_real4 => dict_fget1d_real4
        Procedure :: fget_real4 => dict_fget_real4
        Procedure :: fget1d_real8 => dict_fget1d_real8
        Procedure :: fget_real8 => dict_fget_real8
        Procedure :: fget1d_logical => dict_fget1d_logical
        Procedure :: fget_logical => dict_fget_logical
        Procedure :: fget1d_string => dict_fget1d_string
        Procedure :: fget_string => dict_fget_string
        Procedure :: dict_set, dict_set_a1d, dict_set_node
        Generic :: set => dict_set, dict_set_a1d, dict_set_node
        Procedure :: del => dict_del
        Procedure :: hash => dict_hash
        Procedure :: reset => dict_reset
        Procedure :: print => dict_print
        Procedure :: keys => dict_keys

        Procedure :: update => dict_update
        Procedure :: hasKey => dict_hasKey

        Procedure :: p2node => dict_p2node
        Procedure :: fp2node => dict_fp2node
    End Type

    abstract Interface
        function hash_a2i(a, S) result(rst_idx)
            implicit none
            ! ......................... Argument
            class(*),intent(in) :: a
            integer, intent(in) :: S
            ! ......................... return variables
            integer :: rst_idx
        end function
    end interface


    Interface node
        module procedure node_constructor
        module procedure node_constructor_v
        module procedure node_constructor_v1d
        module procedure node_constructor_k_v
        module procedure node_constructor_k_v1d
    End Interface

    Interface list
        module procedure list_constructor
        module procedure list_constructor_sc
        module procedure list_constructor_a1d
    End Interface

    Interface dict
        module procedure dict_constructor
    End Interface

    Interface assignment(=)
        module procedure node_assign_from_node
    End Interface

    Interface operator(==)
        module procedure eq_node_item_um  ! the left must be type(node)
        module procedure eq_node_item1d_um  ! the left must be type(node)
    End Interface

Contains

# include "rdee_ds.node.inc"

# include "rdee_ds.list.inc"

# include "rdee_ds.dict.inc"


    
End Module