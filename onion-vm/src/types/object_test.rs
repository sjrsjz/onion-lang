#[cfg(test)]
mod tests {
    use super::*;
    use arc_gc::gc::GC;

    #[test]
    fn test_basic_types_creation() {
        let int_obj = OnionObject::Integer(42);
        let float_obj = OnionObject::Float(3.14);
        let string_obj = OnionObject::String("hello".to_string());
        let bool_obj = OnionObject::Boolean(true);
        let null_obj = OnionObject::Null;
        
        assert!(matches!(int_obj, OnionObject::Integer(42)));
        assert!(matches!(float_obj, OnionObject::Float(f) if f == 3.14));
        assert!(matches!(string_obj, OnionObject::String(s) if s == "hello"));
        assert!(matches!(bool_obj, OnionObject::Boolean(true)));
        assert!(matches!(null_obj, OnionObject::Null));
    }

    #[test]
    fn test_type_conversions() {
        let int_obj = OnionObject::Integer(42);
        let float_obj = OnionObject::Float(3.14);
        let string_obj = OnionObject::String("123".to_string());
        let bool_obj = OnionObject::Boolean(true);

        // Test to_integer
        assert_eq!(int_obj.to_integer().unwrap(), 42);
        assert_eq!(float_obj.to_integer().unwrap(), 3);
        assert_eq!(string_obj.to_integer().unwrap(), 123);
        assert_eq!(bool_obj.to_integer().unwrap(), 1);

        // Test to_float
        assert_eq!(int_obj.to_float().unwrap(), 42.0);
        assert_eq!(float_obj.to_float().unwrap(), 3.14);
        assert_eq!(bool_obj.to_float().unwrap(), 1.0);

        // Test to_string
        assert_eq!(int_obj.to_string().unwrap(), "42");
        assert_eq!(float_obj.to_string().unwrap(), "3.14");
        assert_eq!(bool_obj.to_string().unwrap(), "true");

        // Test to_boolean
        assert_eq!(int_obj.to_boolean().unwrap(), true);
        assert_eq!(OnionObject::Integer(0).to_boolean().unwrap(), false);
        assert_eq!(float_obj.to_boolean().unwrap(), true);
        assert_eq!(OnionObject::Float(0.0).to_boolean().unwrap(), false);
        assert_eq!(bool_obj.to_boolean().unwrap(), true);
    }

    #[test]
    fn test_binary_arithmetic_operations() {
        let int1 = OnionObject::Integer(10);
        let int2 = OnionObject::Integer(5);
        let float1 = OnionObject::Float(10.5);
        let float2 = OnionObject::Float(2.5);

        // Addition
        let result = int1.binary_add(&int2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(15)));

        let result = float1.binary_add(&float2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Float(f) if f == 13.0));

        let result = int1.binary_add(&float2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Float(f) if f == 12.5));

        // Subtraction
        let result = int1.binary_sub(&int2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(5)));

        // Multiplication
        let result = int1.binary_mul(&int2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(50)));

        // Division
        let result = int1.binary_div(&int2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(2)));

        // Division by zero
        let zero = OnionObject::Integer(0);
        assert!(int1.binary_div(&zero).is_err());

        // Modulo
        let result = int1.binary_mod(&int2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(0)));

        // Power
        let result = int2.binary_pow(&OnionObject::Integer(2)).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(25)));
    }

    #[test]
    fn test_string_operations() {
        let str1 = OnionObject::String("Hello".to_string());
        let str2 = OnionObject::String(" World".to_string());

        // String concatenation
        let result = str1.binary_add(&str2).unwrap();
        assert!(matches!(result.weak(), OnionObject::String(s) if s == "Hello World"));

        // String length
        let result = str1.len().unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(5)));

        // String indexing
        let result = str1.index_of(1).unwrap();
        assert!(matches!(result.weak(), OnionObject::String(s) if s == "e"));

        // Out of bounds
        assert!(str1.index_of(10).is_err());
    }

    #[test]
    fn test_bytes_operations() {
        let bytes1 = OnionObject::Bytes(vec![1, 2, 3]);
        let bytes2 = OnionObject::Bytes(vec![4, 5]);

        // Bytes concatenation
        let result = bytes1.binary_add(&bytes2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Bytes(b) if b == &vec![1, 2, 3, 4, 5]));

        // Bytes length
        let result = bytes1.len().unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(3)));

        // Bytes indexing
        let result = bytes1.index_of(0).unwrap();
        assert!(matches!(result.weak(), OnionObject::Bytes(b) if b == &vec![1]));
    }

    #[test]
    fn test_range_operations() {
        let range1 = OnionObject::Range(1, 10);
        let range2 = OnionObject::Range(5, 15);

        // Range length
        let result = range1.len().unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(9)));

        // Range addition
        let result = range1.binary_add(&range2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Range(6, 25)));
    }

    #[test]
    fn test_bitwise_operations() {
        let int1 = OnionObject::Integer(12); // 1100 in binary
        let int2 = OnionObject::Integer(10); // 1010 in binary

        // Bitwise AND
        let result = int1.binary_and(&int2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(8))); // 1000

        // Bitwise OR
        let result = int1.binary_or(&int2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(14))); // 1110

        // Bitwise XOR
        let result = int1.binary_xor(&int2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(6))); // 0110

        // Left shift
        let result = int1.binary_shl(&OnionObject::Integer(1)).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(24))); // 11000

        // Right shift
        let result = int1.binary_shr(&OnionObject::Integer(1)).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(6))); // 0110
    }

    #[test]
    fn test_boolean_operations() {
        let bool1 = OnionObject::Boolean(true);
        let bool2 = OnionObject::Boolean(false);

        // Boolean AND
        let result = bool1.binary_and(&bool2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Boolean(false)));

        // Boolean OR
        let result = bool1.binary_or(&bool2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Boolean(true)));
    }

    #[test]
    fn test_comparison_operations() {
        let int1 = OnionObject::Integer(10);
        let int2 = OnionObject::Integer(5);
        let float1 = OnionObject::Float(10.0);

        // Equality
        assert!(int1.binary_eq(&float1).unwrap());
        assert!(!int1.binary_eq(&int2).unwrap());

        // Less than
        assert!(!int1.binary_lt(&int2).unwrap());
        assert!(int2.binary_lt(&int1).unwrap());

        // Greater than
        assert!(int1.binary_gt(&int2).unwrap());
        assert!(!int2.binary_gt(&int1).unwrap());
    }

    #[test]
    fn test_unary_operations() {
        let int_obj = OnionObject::Integer(42);
        let float_obj = OnionObject::Float(-3.14);
        let bool_obj = OnionObject::Boolean(true);

        // Unary negation
        let result = int_obj.unary_neg().unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(-42)));

        let result = float_obj.unary_neg().unwrap();
        assert!(matches!(result.weak(), OnionObject::Float(f) if f == 3.14));

        // Unary not
        let result = bool_obj.unary_not().unwrap();
        assert!(matches!(result.weak(), OnionObject::Boolean(false)));

        let result = int_obj.unary_not().unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(i) if i == !42));
    }

    #[test]
    fn test_equals_and_is_same() {
        let int1 = OnionObject::Integer(42);
        let int2 = OnionObject::Integer(42);
        let int3 = OnionObject::Integer(24);
        let float1 = OnionObject::Float(42.0);

        // Test equals
        assert!(int1.equals(&int2).unwrap());
        assert!(!int1.equals(&int3).unwrap());
        assert!(int1.equals(&float1).unwrap()); // Cross-type equality

        // Test is_same (should be same as equals for non-Mut types)
        assert!(int1.is_same(&int2).unwrap());
        assert!(!int1.is_same(&int3).unwrap());
    }

    #[test]
    fn test_mutable_objects() {
        let mut gc = GC::new();
        
        let int_obj = OnionObject::Integer(42);
        let mut_obj = int_obj.mutablize(&mut gc);

        // Test value extraction
        let value = mut_obj.weak().value().unwrap();
        assert!(matches!(value.weak(), OnionObject::Integer(42)));

        // Test assignment through with_data_mut
        let new_value = OnionObject::Integer(100);
        let result = mut_obj.weak_mut().assign(new_value);
        assert!(result.is_ok());
    }

    #[test]
    fn test_invalid_operations() {
        let string_obj = OnionObject::String("hello".to_string());
        let int_obj = OnionObject::Integer(42);

        // Invalid arithmetic operations
        assert!(string_obj.binary_mul(&int_obj).is_err());
        assert!(string_obj.binary_div(&int_obj).is_err());
        assert!(string_obj.unary_neg().is_err());

        // Invalid type conversions
        let null_obj = OnionObject::Null;
        assert!(null_obj.to_integer().is_err());
        assert!(null_obj.to_float().is_err());

        // Invalid attribute access
        assert!(int_obj.get_attribute(&string_obj).is_err());
    }

    #[test]
    fn test_static_object_wrapper() {
        let int_obj = OnionObject::Integer(42);
        let static_obj = OnionStaticObject::new(int_obj);

        assert!(matches!(static_obj.weak(), OnionObject::Integer(42)));
        assert!(static_obj.arcs.is_none()); // No GC arcs for basic types
    }

    #[test]
    fn test_debug_formatting() {
        let int_obj = OnionObject::Integer(42);
        let string_obj = OnionObject::String("test".to_string());
        let bool_obj = OnionObject::Boolean(true);
        let null_obj = OnionObject::Null;

        let debug_str = format!("{:?}", int_obj);
        assert!(debug_str.contains("Integer(42)"));

        let debug_str = format!("{:?}", string_obj);
        assert!(debug_str.contains("String(test)"));

        let debug_str = format!("{:?}", bool_obj);
        assert!(debug_str.contains("Boolean(true)"));

        let debug_str = format!("{:?}", null_obj);
        assert!(debug_str.contains("Null"));
    }
}
