theory Identity

imports
  "../Dispatcher"
  "HOL-Eisbach.Eisbach"

begin
(*

pragma solidity ^0.5.4;

import "./ERC725.sol";

contract ProxyAccount is ERC725 {
    
    uint256 constant OPERATION_CALL = 0;
    uint256 constant OPERATION_CREATE = 1;

    mapping(bytes32 => bytes32) store;
    
    // the owner
    address public owner;
    
    
    constructor(address _owner) public {
        owner = _owner;
    }

    modifier onlyOwner() {
        require(msg.sender == owner, "only-owner-allowed");
        _;
    }
    
    // ----------------
    // Public functions
    
    function () external payable {}
    
    function changeOwner(address _owner)
        external
        onlyOwner
    {
        owner = _owner;
        emit OwnerChanged(owner);
    }

    function getData(bytes32 _key)
        external
        view
        returns (bytes32 _value)
    {
        return store[_key];
    }

    function setData(bytes32 _key, bytes32 _value)
        external
        onlyOwner
    {
        store[_key] = _value;
        emit DataChanged(_key, _value);
    }

    function execute(uint256 _operationType, address _to, uint256 _value, bytes calldata _data)
        external
        onlyOwner
    {
        if (_operationType == OPERATION_CALL) {
            executeCall(_to, _value, _data);
        } else if (_operationType == OPERATION_CREATE) {
            address newContract = executeCreate(_data);
            emit ContractCreated(newContract);
        } else {
            // We don't want to spend users gas if parametar is wrong
            revert();
        }
    }

    // copied from GnosisSafe
    // https://github.com/gnosis/safe-contracts/blob/v0.0.2-alpha/contracts/base/Executor.sol
    function executeCall(address to, uint256 value, bytes memory data)
        internal
        returns (bool success)
    {
        // solium-disable-next-line security/no-inline-assembly
        assembly {
            success := call(gas, to, value, add(data, 0x20), mload(data), 0, 0)
        }
    }

    // copied from GnosisSafe
    // https://github.com/gnosis/safe-contracts/blob/v0.0.2-alpha/contracts/base/Executor.sol
    function executeCreate(bytes memory data)
        internal
        returns (address newContract)
    {
        // solium-disable-next-line security/no-inline-assembly
        assembly {
            newContract := create(0, add(data, 0x20), mload(data))
        }
    }
}
Compiled with:
sudo  docker build -t solc:v0 .
sudo docker run -it --mount type=bind,source=/home/sid/dev/ethereum/seed/eth-isabelle/example/erc725,target=/home/app/erc725 solc:v0

a6f9dae1: changeOwner(address)
44c028fe: execute(uint256,address,uint256,bytes)
54f6127f: getData(bytes32)
8da5cb5b: owner()
749ebfb8: setData(bytes32,bytes32)

*)
value"(parse_bytecode ''608060405260043610610067576000357c01000000000000000000000000000000000000000000000000000000009004806344c028fe1461006957806354f6127f14610123578063749ebfb8146101725780638da5cb5b146101b7578063a6f9dae11461020e575b005b34801561007557600080fd5b506101216004803603608081101561008c57600080fd5b8101908080359060200190929190803573ffffffffffffffffffffffffffffffffffffffff16906020019092919080359060200190929190803590602001906401000000008111156100dd57600080fd5b8201836020820111156100ef57600080fd5b8035906020019184600183028401116401000000008311171561011157600080fd5b909192939192939050505061025f565b005b34801561012f57600080fd5b5061015c6004803603602081101561014657600080fd5b8101908080359060200190929190505050610432565b6040518082815260200191505060405180910390f35b34801561017e57600080fd5b506101b56004803603604081101561019557600080fd5b81019080803590602001909291908035906020019092919050505061044e565b005b3480156101c357600080fd5b506101cc61055c565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b34801561021a57600080fd5b5061025d6004803603602081101561023157600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050610582565b005b600160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff16141515610324576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260128152602001807f6f6e6c792d6f776e65722d616c6c6f776564000000000000000000000000000081525060200191505060405180910390fd5b60008514156103825761037c848484848080601f016020809104026020016040519081016040528093929190818152602001838380828437600081840152601f19601f820116905080830192505050505050506106f0565b5061042b565b60018514156104255760006103da83838080601f016020809104026020016040519081016040528093929190818152602001838380828437600081840152601f19601f82011690508083019250505050505050610708565b90508073ffffffffffffffffffffffffffffffffffffffff167fcf78cf0d6f3d8371e1075c69c492ab4ec5d8cf23a1a239b6a51a1d00be7ca31260405160405180910390a25061042a565b600080fd5b5b5050505050565b6000806000838152602001908152602001600020549050919050565b600160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff16141515610513576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260128152602001807f6f6e6c792d6f776e65722d616c6c6f776564000000000000000000000000000081525060200191505060405180910390fd5b806000808481526020019081526020016000208190555080827f35553580e4553c909abeb5764e842ce1f93c45f9f614bde2a2ca5f5b7b7dc0fb60405160405180910390a35050565b600160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1681565b600160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff16141515610647576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260128152602001807f6f6e6c792d6f776e65722d616c6c6f776564000000000000000000000000000081525060200191505060405180910390fd5b80600160006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff160217905550600160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff167fa2ea9883a321a3e97b8266c2b078bfeec6d50c711ed71f874a90d500ae2eaf3660405160405180910390a250565b600080600083516020850186885af190509392505050565b60008151602083016000f0905091905056fea165627a7a72305820acb1fc015935f0d96d91c02f9189f2e6084e456e20394c7a09660f410ef1c25b0029'')"

definition insts_ex where
"insts_ex == [Stack (PUSH_N [0x80]), Stack (PUSH_N [0x40]), Memory MSTORE, Stack (PUSH_N [4]), Info CALLDATASIZE, Arith inst_LT,
  Stack (PUSH_N [0, 0x67]), Pc JUMPI, Stack (PUSH_N [0]), Stack CALLDATALOAD,
  Stack (PUSH_N [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]), Swap 0, Arith DIV, Dup 0,
  Stack (PUSH_N [0x44, 0xC0, 0x28, 0xFE]), Arith inst_EQ, Stack (PUSH_N [0, 0x69]), Pc JUMPI, Dup 0,
  Stack (PUSH_N [0x54, 0xF6, 0x12, 0x7F]), Arith inst_EQ, Stack (PUSH_N [1, 0x23]), Pc JUMPI, Dup 0,
  Stack (PUSH_N [0x74, 0x9E, 0xBF, 0xB8]), Arith inst_EQ, Stack (PUSH_N [1, 0x72]), Pc JUMPI, Dup 0,
  Stack (PUSH_N [0x8D, 0xA5, 0xCB, 0x5B]), Arith inst_EQ, Stack (PUSH_N [1, 0xB7]), Pc JUMPI, Dup 0,
  Stack (PUSH_N [0xA6, 0xF9, 0xDA, 0xE1]), Arith inst_EQ, Stack (PUSH_N [2, 0xE]), Pc JUMPI, Pc JUMPDEST, Misc STOP, Pc JUMPDEST,
  Info CALLVALUE, Dup 0, Arith ISZERO, Stack (PUSH_N [0, 0x75]), Pc JUMPI, Stack (PUSH_N [0]), Dup 0, Unknown 0xFD, Pc JUMPDEST, Stack POP,
  Stack (PUSH_N [1, 0x21]), Stack (PUSH_N [4]), Dup 0, Info CALLDATASIZE, Arith SUB, Stack (PUSH_N [0x80]), Dup 1, Arith inst_LT,
  Arith ISZERO, Stack (PUSH_N [0, 0x8C]), Pc JUMPI, Stack (PUSH_N [0]), Dup 0, Unknown 0xFD, Pc JUMPDEST, Dup 1, Arith ADD, Swap 0, Dup 0,
  Dup 0, Stack CALLDATALOAD, Swap 0, Stack (PUSH_N [0x20]), Arith ADD, Swap 0, Swap 2, Swap 1, Swap 0, Dup 0, Stack CALLDATALOAD,
  Stack (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
  Bits inst_AND, Swap 0, Stack (PUSH_N [0x20]), Arith ADD, Swap 0, Swap 2, Swap 1, Swap 0, Dup 0, Stack CALLDATALOAD, Swap 0,
  Stack (PUSH_N [0x20]), Arith ADD, Swap 0, Swap 2, Swap 1, Swap 0, Dup 0, Stack CALLDATALOAD, Swap 0, Stack (PUSH_N [0x20]), Arith ADD,
  Swap 0, Stack (PUSH_N [1, 0, 0, 0, 0]), Dup 1, Arith inst_GT, Arith ISZERO, Stack (PUSH_N [0, 0xDD]), Pc JUMPI, Stack (PUSH_N [0]),
  Dup 0, Unknown 0xFD, Pc JUMPDEST, Dup 2, Arith ADD, Dup 3, Stack (PUSH_N [0x20]), Dup 2, Arith ADD, Arith inst_GT, Arith ISZERO,
  Stack (PUSH_N [0, 0xEF]), Pc JUMPI, Stack (PUSH_N [0]), Dup 0, Unknown 0xFD, Pc JUMPDEST, Dup 0, Stack CALLDATALOAD, Swap 0,
  Stack (PUSH_N [0x20]), Arith ADD, Swap 1, Dup 4, Stack (PUSH_N [1]), Dup 3, Arith MUL, Dup 4, Arith ADD, Arith inst_GT,
  Stack (PUSH_N [1, 0, 0, 0, 0]), Dup 3, Arith inst_GT, Bits inst_OR, Arith ISZERO, Stack (PUSH_N [1, 0x11]), Pc JUMPI, Stack (PUSH_N [0]),
  Dup 0, Unknown 0xFD, Pc JUMPDEST, Swap 0, Swap 1, Swap 2, Swap 3, Swap 1, Swap 2, Swap 3, Swap 0, Stack POP, Stack POP, Stack POP,
  Stack (PUSH_N [2, 0x5F]), Pc JUMP, Pc JUMPDEST, Misc STOP, Pc JUMPDEST, Info CALLVALUE, Dup 0, Arith ISZERO, Stack (PUSH_N [1, 0x2F]),
  Pc JUMPI, Stack (PUSH_N [0]), Dup 0, Unknown 0xFD, Pc JUMPDEST, Stack POP, Stack (PUSH_N [1, 0x5C]), Stack (PUSH_N [4]), Dup 0,
  Info CALLDATASIZE, Arith SUB, Stack (PUSH_N [0x20]), Dup 1, Arith inst_LT, Arith ISZERO, Stack (PUSH_N [1, 0x46]), Pc JUMPI,
  Stack (PUSH_N [0]), Dup 0, Unknown 0xFD, Pc JUMPDEST, Dup 1, Arith ADD, Swap 0, Dup 0, Dup 0, Stack CALLDATALOAD, Swap 0,
  Stack (PUSH_N [0x20]), Arith ADD, Swap 0, Swap 2, Swap 1, Swap 0, Stack POP, Stack POP, Stack POP, Stack (PUSH_N [4, 0x32]), Pc JUMP,
  Pc JUMPDEST, Stack (PUSH_N [0x40]), Memory MLOAD, Dup 0, Dup 2, Dup 1, Memory MSTORE, Stack (PUSH_N [0x20]), Arith ADD, Swap 1,
  Stack POP, Stack POP, Stack (PUSH_N [0x40]), Memory MLOAD, Dup 0, Swap 1, Arith SUB, Swap 0, Misc RETURN, Pc JUMPDEST, Info CALLVALUE,
  Dup 0, Arith ISZERO, Stack (PUSH_N [1, 0x7E]), Pc JUMPI, Stack (PUSH_N [0]), Dup 0, Unknown 0xFD, Pc JUMPDEST, Stack POP,
  Stack (PUSH_N [1, 0xB5]), Stack (PUSH_N [4]), Dup 0, Info CALLDATASIZE, Arith SUB, Stack (PUSH_N [0x40]), Dup 1, Arith inst_LT,
  Arith ISZERO, Stack (PUSH_N [1, 0x95]), Pc JUMPI, Stack (PUSH_N [0]), Dup 0, Unknown 0xFD, Pc JUMPDEST, Dup 1, Arith ADD, Swap 0, Dup 0,
  Dup 0, Stack CALLDATALOAD, Swap 0, Stack (PUSH_N [0x20]), Arith ADD, Swap 0, Swap 2, Swap 1, Swap 0, Dup 0, Stack CALLDATALOAD, Swap 0,
  Stack (PUSH_N [0x20]), Arith ADD, Swap 0, Swap 2, Swap 1, Swap 0, Stack POP, Stack POP, Stack POP, Stack (PUSH_N [4, 0x4E]), Pc JUMP,
  Pc JUMPDEST, Misc STOP, Pc JUMPDEST, Info CALLVALUE, Dup 0, Arith ISZERO, Stack (PUSH_N [1, 0xC3]), Pc JUMPI, Stack (PUSH_N [0]), Dup 0,
  Unknown 0xFD, Pc JUMPDEST, Stack POP, Stack (PUSH_N [1, 0xCC]), Stack (PUSH_N [5, 0x5C]), Pc JUMP, Pc JUMPDEST, Stack (PUSH_N [0x40]),
  Memory MLOAD, Dup 0, Dup 2,
  Stack (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
  Bits inst_AND,
  Stack (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
  Bits inst_AND, Dup 1, Memory MSTORE, Stack (PUSH_N [0x20]), Arith ADD, Swap 1, Stack POP, Stack POP, Stack (PUSH_N [0x40]), Memory MLOAD,
  Dup 0, Swap 1, Arith SUB, Swap 0, Misc RETURN, Pc JUMPDEST, Info CALLVALUE, Dup 0, Arith ISZERO, Stack (PUSH_N [2, 0x1A]), Pc JUMPI,
  Stack (PUSH_N [0]), Dup 0, Unknown 0xFD, Pc JUMPDEST, Stack POP, Stack (PUSH_N [2, 0x5D]), Stack (PUSH_N [4]), Dup 0, Info CALLDATASIZE,
  Arith SUB, Stack (PUSH_N [0x20]), Dup 1, Arith inst_LT, Arith ISZERO, Stack (PUSH_N [2, 0x31]), Pc JUMPI, Stack (PUSH_N [0]), Dup 0,
  Unknown 0xFD, Pc JUMPDEST, Dup 1, Arith ADD, Swap 0, Dup 0, Dup 0, Stack CALLDATALOAD,
  Stack (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
  Bits inst_AND, Swap 0, Stack (PUSH_N [0x20]), Arith ADD, Swap 0, Swap 2, Swap 1, Swap 0, Stack POP, Stack POP, Stack POP,
  Stack (PUSH_N [5, 0x82]), Pc JUMP, Pc JUMPDEST, Misc STOP, Pc JUMPDEST, Stack (PUSH_N [1]), Stack (PUSH_N [0]), Swap 0, Storage SLOAD,
  Swap 0, Stack (PUSH_N [1, 0]), Arith EXP, Swap 0, Arith DIV,
  Stack (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
  Bits inst_AND,
  Stack (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
  Bits inst_AND, Info CALLER,
  Stack (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
  Bits inst_AND, Arith inst_EQ, Arith ISZERO, Arith ISZERO, Stack (PUSH_N [3, 0x24]), Pc JUMPI, Stack (PUSH_N [0x40]), Memory MLOAD,
  Stack (PUSH_N [8, 0xC3, 0x79, 0xA0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]), Dup 1,
  Memory MSTORE, Stack (PUSH_N [4]), Arith ADD, Dup 0, Dup 0, Stack (PUSH_N [0x20]), Arith ADD, Dup 2, Dup 1, Arith SUB, Dup 2,
  Memory MSTORE, Stack (PUSH_N [0x12]), Dup 1, Memory MSTORE, Stack (PUSH_N [0x20]), Arith ADD, Dup 0,
  Stack
   (PUSH_N
     [0x6F, 0x6E, 0x6C, 0x79, 0x2D, 0x6F, 0x77, 0x6E, 0x65, 0x72, 0x2D, 0x61, 0x6C, 0x6C, 0x6F, 0x77, 0x65, 0x64, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0]),
  Dup 1, Memory MSTORE, Stack POP, Stack (PUSH_N [0x20]), Arith ADD, Swap 1, Stack POP, Stack POP, Stack (PUSH_N [0x40]), Memory MLOAD,
  Dup 0, Swap 1, Arith SUB, Swap 0, Unknown 0xFD, Pc JUMPDEST, Stack (PUSH_N [0]), Dup 5, Arith inst_EQ, Arith ISZERO,
  Stack (PUSH_N [3, 0x82]), Pc JUMPI, Stack (PUSH_N [3, 0x7C]), Dup 4, Dup 4, Dup 4, Dup 4, Dup 0, Dup 0, Stack (PUSH_N [0x1F]), Arith ADD,
  Stack (PUSH_N [0x20]), Dup 0, Swap 1, Arith DIV, Arith MUL, Stack (PUSH_N [0x20]), Arith ADD, Stack (PUSH_N [0x40]), Memory MLOAD,
  Swap 0, Dup 1, Arith ADD, Stack (PUSH_N [0x40]), Memory MSTORE, Dup 0, Swap 3, Swap 2, Swap 1, Swap 0, Dup 1, Dup 1, Memory MSTORE,
  Stack (PUSH_N [0x20]), Arith ADD, Dup 3, Dup 3, Dup 0, Dup 2, Dup 4, Memory CALLDATACOPY, Stack (PUSH_N [0]), Dup 1, Dup 4, Arith ADD,
  Memory MSTORE, Stack (PUSH_N [0x1F]), Bits inst_NOT, Stack (PUSH_N [0x1F]), Dup 2, Arith ADD, Bits inst_AND, Swap 0, Stack POP, Dup 0,
  Dup 3, Arith ADD, Swap 2, Stack POP, Stack POP, Stack POP, Stack POP, Stack POP, Stack POP, Stack POP, Stack (PUSH_N [6, 0xF0]), Pc JUMP,
  Pc JUMPDEST, Stack POP, Stack (PUSH_N [4, 0x2B]), Pc JUMP, Pc JUMPDEST, Stack (PUSH_N [1]), Dup 5, Arith inst_EQ, Arith ISZERO,
  Stack (PUSH_N [4, 0x25]), Pc JUMPI, Stack (PUSH_N [0]), Stack (PUSH_N [3, 0xDA]), Dup 3, Dup 3, Dup 0, Dup 0, Stack (PUSH_N [0x1F]),
  Arith ADD, Stack (PUSH_N [0x20]), Dup 0, Swap 1, Arith DIV, Arith MUL, Stack (PUSH_N [0x20]), Arith ADD, Stack (PUSH_N [0x40]),
  Memory MLOAD, Swap 0, Dup 1, Arith ADD, Stack (PUSH_N [0x40]), Memory MSTORE, Dup 0, Swap 3, Swap 2, Swap 1, Swap 0, Dup 1, Dup 1,
  Memory MSTORE, Stack (PUSH_N [0x20]), Arith ADD, Dup 3, Dup 3, Dup 0, Dup 2, Dup 4, Memory CALLDATACOPY, Stack (PUSH_N [0]), Dup 1,
  Dup 4, Arith ADD, Memory MSTORE, Stack (PUSH_N [0x1F]), Bits inst_NOT, Stack (PUSH_N [0x1F]), Dup 2, Arith ADD, Bits inst_AND, Swap 0,
  Stack POP, Dup 0, Dup 3, Arith ADD, Swap 2, Stack POP, Stack POP, Stack POP, Stack POP, Stack POP, Stack POP, Stack POP,
  Stack (PUSH_N [7, 8]), Pc JUMP, Pc JUMPDEST, Swap 0, Stack POP, Dup 0,
  Stack (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
  Bits inst_AND,
  Stack
   (PUSH_N
     [0xCF, 0x78, 0xCF, 0xD, 0x6F, 0x3D, 0x83, 0x71, 0xE1, 7, 0x5C, 0x69, 0xC4, 0x92, 0xAB, 0x4E, 0xC5, 0xD8, 0xCF, 0x23, 0xA1, 0xA2, 0x39,
      0xB6, 0xA5, 0x1A, 0x1D, 0, 0xBE, 0x7C, 0xA3, 0x12]),
  Stack (PUSH_N [0x40]), Memory MLOAD, Stack (PUSH_N [0x40]), Memory MLOAD, Dup 0, Swap 1, Arith SUB, Swap 0, Log LOG2, Stack POP,
  Stack (PUSH_N [4, 0x2A]), Pc JUMP, Pc JUMPDEST, Stack (PUSH_N [0]), Dup 0, Unknown 0xFD, Pc JUMPDEST, Pc JUMPDEST, Stack POP, Stack POP,
  Stack POP, Stack POP, Stack POP, Pc JUMP, Pc JUMPDEST, Stack (PUSH_N [0]), Dup 0, Stack (PUSH_N [0]), Dup 3, Dup 1, Memory MSTORE,
  Stack (PUSH_N [0x20]), Arith ADD, Swap 0, Dup 1, Memory MSTORE, Stack (PUSH_N [0x20]), Arith ADD, Stack (PUSH_N [0]), Arith SHA3,
  Storage SLOAD, Swap 0, Stack POP, Swap 1, Swap 0, Stack POP, Pc JUMP, Pc JUMPDEST, Stack (PUSH_N [1]), Stack (PUSH_N [0]), Swap 0,
  Storage SLOAD, Swap 0, Stack (PUSH_N [1, 0]), Arith EXP, Swap 0, Arith DIV,
  Stack (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
  Bits inst_AND,
  Stack (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
  Bits inst_AND, Info CALLER,
  Stack (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
  Bits inst_AND, Arith inst_EQ, Arith ISZERO, Arith ISZERO, Stack (PUSH_N [5, 0x13]), Pc JUMPI, Stack (PUSH_N [0x40]), Memory MLOAD,
  Stack (PUSH_N [8, 0xC3, 0x79, 0xA0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]), Dup 1,
  Memory MSTORE, Stack (PUSH_N [4]), Arith ADD, Dup 0, Dup 0, Stack (PUSH_N [0x20]), Arith ADD, Dup 2, Dup 1, Arith SUB, Dup 2,
  Memory MSTORE, Stack (PUSH_N [0x12]), Dup 1, Memory MSTORE, Stack (PUSH_N [0x20]), Arith ADD, Dup 0,
  Stack
   (PUSH_N
     [0x6F, 0x6E, 0x6C, 0x79, 0x2D, 0x6F, 0x77, 0x6E, 0x65, 0x72, 0x2D, 0x61, 0x6C, 0x6C, 0x6F, 0x77, 0x65, 0x64, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0]),
  Dup 1, Memory MSTORE, Stack POP, Stack (PUSH_N [0x20]), Arith ADD, Swap 1, Stack POP, Stack POP, Stack (PUSH_N [0x40]), Memory MLOAD,
  Dup 0, Swap 1, Arith SUB, Swap 0, Unknown 0xFD, Pc JUMPDEST, Dup 0, Stack (PUSH_N [0]), Dup 0, Dup 4, Dup 1, Memory MSTORE,
  Stack (PUSH_N [0x20]), Arith ADD, Swap 0, Dup 1, Memory MSTORE, Stack (PUSH_N [0x20]), Arith ADD, Stack (PUSH_N [0]), Arith SHA3, Dup 1,
  Swap 0, Storage SSTORE, Stack POP, Dup 0, Dup 2,
  Stack
   (PUSH_N
     [0x35, 0x55, 0x35, 0x80, 0xE4, 0x55, 0x3C, 0x90, 0x9A, 0xBE, 0xB5, 0x76, 0x4E, 0x84, 0x2C, 0xE1, 0xF9, 0x3C, 0x45, 0xF9, 0xF6, 0x14,
      0xBD, 0xE2, 0xA2, 0xCA, 0x5F, 0x5B, 0x7B, 0x7D, 0xC0, 0xFB]),
  Stack (PUSH_N [0x40]), Memory MLOAD, Stack (PUSH_N [0x40]), Memory MLOAD, Dup 0, Swap 1, Arith SUB, Swap 0, Log LOG3, Stack POP,
  Stack POP, Pc JUMP, Pc JUMPDEST, Stack (PUSH_N [1]), Stack (PUSH_N [0]), Swap 0, Storage SLOAD, Swap 0, Stack (PUSH_N [1, 0]), Arith EXP,
  Swap 0, Arith DIV,
  Stack (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
  Bits inst_AND, Dup 1, Pc JUMP, Pc JUMPDEST, Stack (PUSH_N [1]), Stack (PUSH_N [0]), Swap 0, Storage SLOAD, Swap 0, Stack (PUSH_N [1, 0]),
  Arith EXP, Swap 0, Arith DIV,
  Stack (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
  Bits inst_AND,
  Stack (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
  Bits inst_AND, Info CALLER,
  Stack (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
  Bits inst_AND, Arith inst_EQ, Arith ISZERO, Arith ISZERO, Stack (PUSH_N [6, 0x47]), Pc JUMPI, Stack (PUSH_N [0x40]), Memory MLOAD,
  Stack (PUSH_N [8, 0xC3, 0x79, 0xA0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]), Dup 1,
  Memory MSTORE, Stack (PUSH_N [4]), Arith ADD, Dup 0, Dup 0, Stack (PUSH_N [0x20]), Arith ADD, Dup 2, Dup 1, Arith SUB, Dup 2,
  Memory MSTORE, Stack (PUSH_N [0x12]), Dup 1, Memory MSTORE, Stack (PUSH_N [0x20]), Arith ADD, Dup 0,
  Stack
   (PUSH_N
     [0x6F, 0x6E, 0x6C, 0x79, 0x2D, 0x6F, 0x77, 0x6E, 0x65, 0x72, 0x2D, 0x61, 0x6C, 0x6C, 0x6F, 0x77, 0x65, 0x64, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0]),
  Dup 1, Memory MSTORE, Stack POP, Stack (PUSH_N [0x20]), Arith ADD, Swap 1, Stack POP, Stack POP, Stack (PUSH_N [0x40]), Memory MLOAD,
  Dup 0, Swap 1, Arith SUB, Swap 0, Unknown 0xFD, Pc JUMPDEST, Dup 0, Stack (PUSH_N [1]), Stack (PUSH_N [0]), Stack (PUSH_N [1, 0]),
  Arith EXP, Dup 1, Storage SLOAD, Dup 1,
  Stack (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
  Arith MUL, Bits inst_NOT, Bits inst_AND, Swap 0, Dup 3,
  Stack (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
  Bits inst_AND, Arith MUL, Bits inst_OR, Swap 0, Storage SSTORE, Stack POP, Stack (PUSH_N [1]), Stack (PUSH_N [0]), Swap 0, Storage SLOAD,
  Swap 0, Stack (PUSH_N [1, 0]), Arith EXP, Swap 0, Arith DIV,
  Stack (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
  Bits inst_AND,
  Stack (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
  Bits inst_AND,
  Stack
   (PUSH_N
     [0xA2, 0xEA, 0x98, 0x83, 0xA3, 0x21, 0xA3, 0xE9, 0x7B, 0x82, 0x66, 0xC2, 0xB0, 0x78, 0xBF, 0xEE, 0xC6, 0xD5, 0xC, 0x71, 0x1E, 0xD7,
      0x1F, 0x87, 0x4A, 0x90, 0xD5, 0, 0xAE, 0x2E, 0xAF, 0x36]),
  Stack (PUSH_N [0x40]), Memory MLOAD, Stack (PUSH_N [0x40]), Memory MLOAD, Dup 0, Swap 1, Arith SUB, Swap 0, Log LOG2, Stack POP, Pc JUMP,
  Pc JUMPDEST, Stack (PUSH_N [0]), Dup 0, Stack (PUSH_N [0]), Dup 3, Memory MLOAD, Stack (PUSH_N [0x20]), Dup 5, Arith ADD, Dup 6, Dup 8,
  Info GAS, Misc CALL, Swap 0, Stack POP, Swap 3, Swap 2, Stack POP, Stack POP, Stack POP, Pc JUMP, Pc JUMPDEST, Stack (PUSH_N [0]), Dup 1,
  Memory MLOAD, Stack (PUSH_N [0x20]), Dup 3, Arith ADD, Stack (PUSH_N [0]), Misc CREATE, Swap 0, Stack POP, Swap 1, Swap 0, Stack POP,
  Pc JUMP, Unknown 0xFE, Log LOG1, Stack (PUSH_N [0x62, 0x7A, 0x7A, 0x72, 0x30, 0x58]), Arith SHA3, Unknown 0xAC, Unknown 0xB1,
  Unknown 0xFC, Arith ADD, Memory MSIZE, Stack CALLDATALOAD, Misc CREATE, Unknown 0xD9,
  Stack (PUSH_N [0x91, 0xC0, 0x2F, 0x91, 0x89, 0xF2, 0xE6, 8, 0x4E, 0x45, 0x6E, 0x20, 0x39, 0x4C]),
  Stack (PUSH_N [9, 0x66, 0xF, 0x41, 0xE, 0xF1, 0xC2, 0x5B, 0, 0x29])]"
value "length insts_ex"
(* 898 instructions *)

lemma
 "parse_bytecode ''608060405260043610610067576000357c01000000000000000000000000000000000000000000000000000000009004806344c028fe1461006957806354f6127f14610123578063749ebfb8146101725780638da5cb5b146101b7578063a6f9dae11461020e575b005b34801561007557600080fd5b506101216004803603608081101561008c57600080fd5b8101908080359060200190929190803573ffffffffffffffffffffffffffffffffffffffff16906020019092919080359060200190929190803590602001906401000000008111156100dd57600080fd5b8201836020820111156100ef57600080fd5b8035906020019184600183028401116401000000008311171561011157600080fd5b909192939192939050505061025f565b005b34801561012f57600080fd5b5061015c6004803603602081101561014657600080fd5b8101908080359060200190929190505050610432565b6040518082815260200191505060405180910390f35b34801561017e57600080fd5b506101b56004803603604081101561019557600080fd5b81019080803590602001909291908035906020019092919050505061044e565b005b3480156101c357600080fd5b506101cc61055c565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b34801561021a57600080fd5b5061025d6004803603602081101561023157600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050610582565b005b600160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff16141515610324576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260128152602001807f6f6e6c792d6f776e65722d616c6c6f776564000000000000000000000000000081525060200191505060405180910390fd5b60008514156103825761037c848484848080601f016020809104026020016040519081016040528093929190818152602001838380828437600081840152601f19601f820116905080830192505050505050506106f0565b5061042b565b60018514156104255760006103da83838080601f016020809104026020016040519081016040528093929190818152602001838380828437600081840152601f19601f82011690508083019250505050505050610708565b90508073ffffffffffffffffffffffffffffffffffffffff167fcf78cf0d6f3d8371e1075c69c492ab4ec5d8cf23a1a239b6a51a1d00be7ca31260405160405180910390a25061042a565b600080fd5b5b5050505050565b6000806000838152602001908152602001600020549050919050565b600160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff16141515610513576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260128152602001807f6f6e6c792d6f776e65722d616c6c6f776564000000000000000000000000000081525060200191505060405180910390fd5b806000808481526020019081526020016000208190555080827f35553580e4553c909abeb5764e842ce1f93c45f9f614bde2a2ca5f5b7b7dc0fb60405160405180910390a35050565b600160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1681565b600160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff16141515610647576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260128152602001807f6f6e6c792d6f776e65722d616c6c6f776564000000000000000000000000000081525060200191505060405180910390fd5b80600160006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff160217905550600160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff167fa2ea9883a321a3e97b8266c2b078bfeec6d50c711ed71f874a90d500ae2eaf3660405160405180910390a250565b600080600083516020850186885af190509392505050565b60008151602083016000f0905091905056fea165627a7a72305820acb1fc015935f0d96d91c02f9189f2e6084e456e20394c7a09660f410ef1c25b0029'' = insts_ex"
  unfolding insts_ex_def
  by eval

definition "blocks_identity == build_blocks insts_ex"
value "blocks_identity"
lemma blocks_identity_simp:
 "blocks_identity = [(0, [(0, Stack (PUSH_N [0x80])), (2, Stack (PUSH_N [0x40])), (4, Memory MSTORE), (5, Stack (PUSH_N [4])), (7, Info CALLDATASIZE),
       (8, Arith inst_LT), (9, Stack (PUSH_N [0, 0x67]))],
   Jumpi),
  (13,
   [(13, Stack (PUSH_N [0])), (15, Stack CALLDATALOAD),
    (16, Stack (PUSH_N [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])), (46, Swap 0),
    (47, Arith DIV), (48, Dup 0), (49, Stack (PUSH_N [0x44, 0xC0, 0x28, 0xFE])), (54, Arith inst_EQ), (55, Stack (PUSH_N [0, 0x69]))],
   Jumpi),
  (59, [(59, Dup 0), (60, Stack (PUSH_N [0x54, 0xF6, 0x12, 0x7F])), (65, Arith inst_EQ), (66, Stack (PUSH_N [1, 0x23]))], Jumpi),
  (70, [(70, Dup 0), (71, Stack (PUSH_N [0x74, 0x9E, 0xBF, 0xB8])), (76, Arith inst_EQ), (77, Stack (PUSH_N [1, 0x72]))], Jumpi),
  (81, [(81, Dup 0), (82, Stack (PUSH_N [0x8D, 0xA5, 0xCB, 0x5B])), (87, Arith inst_EQ), (88, Stack (PUSH_N [1, 0xB7]))], Jumpi),
  (92, [(92, Dup 0), (93, Stack (PUSH_N [0xA6, 0xF9, 0xDA, 0xE1])), (98, Arith inst_EQ), (99, Stack (PUSH_N [2, 0xE]))], Jumpi),
  (103, [(103, Pc JUMPDEST), (104, Misc STOP)], Terminal),
  (105, [(105, Pc JUMPDEST), (106, Info CALLVALUE), (107, Dup 0), (108, Arith ISZERO), (109, Stack (PUSH_N [0, 0x75]))], Jumpi),
  (113, [(113, Stack (PUSH_N [0])), (115, Dup 0), (116, Unknown 0xFD)], Terminal),
  (117,
   [(117, Pc JUMPDEST), (118, Stack POP), (119, Stack (PUSH_N [1, 0x21])), (122, Stack (PUSH_N [4])), (124, Dup 0),
    (125, Info CALLDATASIZE), (126, Arith SUB), (127, Stack (PUSH_N [0x80])), (129, Dup 1), (130, Arith inst_LT), (131, Arith ISZERO),
    (132, Stack (PUSH_N [0, 0x8C]))],
   Jumpi),
  (136, [(136, Stack (PUSH_N [0])), (138, Dup 0), (139, Unknown 0xFD)], Terminal),
  (140,
   [(140, Pc JUMPDEST), (141, Dup 1), (142, Arith ADD), (143, Swap 0), (144, Dup 0), (145, Dup 0), (146, Stack CALLDATALOAD),
    (147, Swap 0), (148, Stack (PUSH_N [0x20])), (150, Arith ADD), (151, Swap 0), (152, Swap 2), (153, Swap 1), (154, Swap 0),
    (155, Dup 0), (156, Stack CALLDATALOAD),
    (157,
     Stack
      (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
    (178, Bits inst_AND), (179, Swap 0), (180, Stack (PUSH_N [0x20])), (182, Arith ADD), (183, Swap 0), (184, Swap 2), (185, Swap 1),
    (186, Swap 0), (187, Dup 0), (188, Stack CALLDATALOAD), (189, Swap 0), (190, Stack (PUSH_N [0x20])), (192, Arith ADD), (193, Swap 0),
    (194, Swap 2), (195, Swap 1), (196, Swap 0), (197, Dup 0), (198, Stack CALLDATALOAD), (199, Swap 0), (200, Stack (PUSH_N [0x20])),
    (202, Arith ADD), (203, Swap 0), (204, Stack (PUSH_N [1, 0, 0, 0, 0])), (210, Dup 1), (211, Arith inst_GT), (212, Arith ISZERO),
    (213, Stack (PUSH_N [0, 0xDD]))],
   Jumpi),
  (217, [(217, Stack (PUSH_N [0])), (219, Dup 0), (220, Unknown 0xFD)], Terminal),
  (221,
   [(221, Pc JUMPDEST), (222, Dup 2), (223, Arith ADD), (224, Dup 3), (225, Stack (PUSH_N [0x20])), (227, Dup 2), (228, Arith ADD),
    (229, Arith inst_GT), (230, Arith ISZERO), (231, Stack (PUSH_N [0, 0xEF]))],
   Jumpi),
  (235, [(235, Stack (PUSH_N [0])), (237, Dup 0), (238, Unknown 0xFD)], Terminal),
  (239,
   [(239, Pc JUMPDEST), (240, Dup 0), (241, Stack CALLDATALOAD), (242, Swap 0), (243, Stack (PUSH_N [0x20])), (245, Arith ADD),
    (246, Swap 1), (247, Dup 4), (248, Stack (PUSH_N [1])), (250, Dup 3), (251, Arith MUL), (252, Dup 4), (253, Arith ADD),
    (254, Arith inst_GT), (255, Stack (PUSH_N [1, 0, 0, 0, 0])), (261, Dup 3), (262, Arith inst_GT), (263, Bits inst_OR),
    (264, Arith ISZERO), (265, Stack (PUSH_N [1, 0x11]))],
   Jumpi),
  (269, [(269, Stack (PUSH_N [0])), (271, Dup 0), (272, Unknown 0xFD)], Terminal),
  (273,
   [(273, Pc JUMPDEST), (274, Swap 0), (275, Swap 1), (276, Swap 2), (277, Swap 3), (278, Swap 1), (279, Swap 2), (280, Swap 3),
    (281, Swap 0), (282, Stack POP), (283, Stack POP), (284, Stack POP), (285, Stack (PUSH_N [2, 0x5F]))],
   Jump),
  (289, [(289, Pc JUMPDEST), (290, Misc STOP)], Terminal),
  (291, [(291, Pc JUMPDEST), (292, Info CALLVALUE), (293, Dup 0), (294, Arith ISZERO), (295, Stack (PUSH_N [1, 0x2F]))], Jumpi),
  (299, [(299, Stack (PUSH_N [0])), (301, Dup 0), (302, Unknown 0xFD)], Terminal),
  (303,
   [(303, Pc JUMPDEST), (304, Stack POP), (305, Stack (PUSH_N [1, 0x5C])), (308, Stack (PUSH_N [4])), (310, Dup 0),
    (311, Info CALLDATASIZE), (312, Arith SUB), (313, Stack (PUSH_N [0x20])), (315, Dup 1), (316, Arith inst_LT), (317, Arith ISZERO),
    (318, Stack (PUSH_N [1, 0x46]))],
   Jumpi),
  (322, [(322, Stack (PUSH_N [0])), (324, Dup 0), (325, Unknown 0xFD)], Terminal),
  (326,
   [(326, Pc JUMPDEST), (327, Dup 1), (328, Arith ADD), (329, Swap 0), (330, Dup 0), (331, Dup 0), (332, Stack CALLDATALOAD),
    (333, Swap 0), (334, Stack (PUSH_N [0x20])), (336, Arith ADD), (337, Swap 0), (338, Swap 2), (339, Swap 1), (340, Swap 0),
    (341, Stack POP), (342, Stack POP), (343, Stack POP), (344, Stack (PUSH_N [4, 0x32]))],
   Jump),
  (348,
   [(348, Pc JUMPDEST), (349, Stack (PUSH_N [0x40])), (351, Memory MLOAD), (352, Dup 0), (353, Dup 2), (354, Dup 1), (355, Memory MSTORE),
    (356, Stack (PUSH_N [0x20])), (358, Arith ADD), (359, Swap 1), (360, Stack POP), (361, Stack POP), (362, Stack (PUSH_N [0x40])),
    (364, Memory MLOAD), (365, Dup 0), (366, Swap 1), (367, Arith SUB), (368, Swap 0), (369, Misc RETURN)],
   Terminal),
  (370, [(370, Pc JUMPDEST), (371, Info CALLVALUE), (372, Dup 0), (373, Arith ISZERO), (374, Stack (PUSH_N [1, 0x7E]))], Jumpi),
  (378, [(378, Stack (PUSH_N [0])), (380, Dup 0), (381, Unknown 0xFD)], Terminal),
  (382,
   [(382, Pc JUMPDEST), (383, Stack POP), (384, Stack (PUSH_N [1, 0xB5])), (387, Stack (PUSH_N [4])), (389, Dup 0),
    (390, Info CALLDATASIZE), (391, Arith SUB), (392, Stack (PUSH_N [0x40])), (394, Dup 1), (395, Arith inst_LT), (396, Arith ISZERO),
    (397, Stack (PUSH_N [1, 0x95]))],
   Jumpi),
  (401, [(401, Stack (PUSH_N [0])), (403, Dup 0), (404, Unknown 0xFD)], Terminal),
  (405,
   [(405, Pc JUMPDEST), (406, Dup 1), (407, Arith ADD), (408, Swap 0), (409, Dup 0), (410, Dup 0), (411, Stack CALLDATALOAD),
    (412, Swap 0), (413, Stack (PUSH_N [0x20])), (415, Arith ADD), (416, Swap 0), (417, Swap 2), (418, Swap 1), (419, Swap 0),
    (420, Dup 0), (421, Stack CALLDATALOAD), (422, Swap 0), (423, Stack (PUSH_N [0x20])), (425, Arith ADD), (426, Swap 0), (427, Swap 2),
    (428, Swap 1), (429, Swap 0), (430, Stack POP), (431, Stack POP), (432, Stack POP), (433, Stack (PUSH_N [4, 0x4E]))],
   Jump),
  (437, [(437, Pc JUMPDEST), (438, Misc STOP)], Terminal),
  (439, [(439, Pc JUMPDEST), (440, Info CALLVALUE), (441, Dup 0), (442, Arith ISZERO), (443, Stack (PUSH_N [1, 0xC3]))], Jumpi),
  (447, [(447, Stack (PUSH_N [0])), (449, Dup 0), (450, Unknown 0xFD)], Terminal),
  (451, [(451, Pc JUMPDEST), (452, Stack POP), (453, Stack (PUSH_N [1, 0xCC])), (456, Stack (PUSH_N [5, 0x5C]))], Jump),
  (460,
   [(460, Pc JUMPDEST), (461, Stack (PUSH_N [0x40])), (463, Memory MLOAD), (464, Dup 0), (465, Dup 2),
    (466,
     Stack
      (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
    (487, Bits inst_AND),
    (488,
     Stack
      (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
    (509, Bits inst_AND), (510, Dup 1), (511, Memory MSTORE), (512, Stack (PUSH_N [0x20])), (514, Arith ADD), (515, Swap 1),
    (516, Stack POP), (517, Stack POP), (518, Stack (PUSH_N [0x40])), (520, Memory MLOAD), (521, Dup 0), (522, Swap 1), (523, Arith SUB),
    (524, Swap 0), (525, Misc RETURN)],
   Terminal),
  (526, [(526, Pc JUMPDEST), (527, Info CALLVALUE), (528, Dup 0), (529, Arith ISZERO), (530, Stack (PUSH_N [2, 0x1A]))], Jumpi),
  (534, [(534, Stack (PUSH_N [0])), (536, Dup 0), (537, Unknown 0xFD)], Terminal),
  (538,
   [(538, Pc JUMPDEST), (539, Stack POP), (540, Stack (PUSH_N [2, 0x5D])), (543, Stack (PUSH_N [4])), (545, Dup 0),
    (546, Info CALLDATASIZE), (547, Arith SUB), (548, Stack (PUSH_N [0x20])), (550, Dup 1), (551, Arith inst_LT), (552, Arith ISZERO),
    (553, Stack (PUSH_N [2, 0x31]))],
   Jumpi),
  (557, [(557, Stack (PUSH_N [0])), (559, Dup 0), (560, Unknown 0xFD)], Terminal),
  (561,
   [(561, Pc JUMPDEST), (562, Dup 1), (563, Arith ADD), (564, Swap 0), (565, Dup 0), (566, Dup 0), (567, Stack CALLDATALOAD),
    (568,
     Stack
      (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
    (589, Bits inst_AND), (590, Swap 0), (591, Stack (PUSH_N [0x20])), (593, Arith ADD), (594, Swap 0), (595, Swap 2), (596, Swap 1),
    (597, Swap 0), (598, Stack POP), (599, Stack POP), (600, Stack POP), (601, Stack (PUSH_N [5, 0x82]))],
   Jump),
  (605, [(605, Pc JUMPDEST), (606, Misc STOP)], Terminal),
  (607,
   [(607, Pc JUMPDEST), (608, Stack (PUSH_N [1])), (610, Stack (PUSH_N [0])), (612, Swap 0), (613, Storage SLOAD), (614, Swap 0),
    (615, Stack (PUSH_N [1, 0])), (618, Arith EXP), (619, Swap 0), (620, Arith DIV),
    (621,
     Stack
      (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
    (642, Bits inst_AND),
    (643,
     Stack
      (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
    (664, Bits inst_AND), (665, Info CALLER),
    (666,
     Stack
      (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
    (687, Bits inst_AND), (688, Arith inst_EQ), (689, Arith ISZERO), (690, Arith ISZERO), (691, Stack (PUSH_N [3, 0x24]))],
   Jumpi),
  (695,
   [(695, Stack (PUSH_N [0x40])), (697, Memory MLOAD),
    (698, Stack (PUSH_N [8, 0xC3, 0x79, 0xA0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])),
    (731, Dup 1), (732, Memory MSTORE), (733, Stack (PUSH_N [4])), (735, Arith ADD), (736, Dup 0), (737, Dup 0),
    (738, Stack (PUSH_N [0x20])), (740, Arith ADD), (741, Dup 2), (742, Dup 1), (743, Arith SUB), (744, Dup 2), (745, Memory MSTORE),
    (746, Stack (PUSH_N [0x12])), (748, Dup 1), (749, Memory MSTORE), (750, Stack (PUSH_N [0x20])), (752, Arith ADD), (753, Dup 0),
    (754,
     Stack
      (PUSH_N
        [0x6F, 0x6E, 0x6C, 0x79, 0x2D, 0x6F, 0x77, 0x6E, 0x65, 0x72, 0x2D, 0x61, 0x6C, 0x6C, 0x6F, 0x77, 0x65, 0x64, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0])),
    (787, Dup 1), (788, Memory MSTORE), (789, Stack POP), (790, Stack (PUSH_N [0x20])), (792, Arith ADD), (793, Swap 1), (794, Stack POP),
    (795, Stack POP), (796, Stack (PUSH_N [0x40])), (798, Memory MLOAD), (799, Dup 0), (800, Swap 1), (801, Arith SUB), (802, Swap 0),
    (803, Unknown 0xFD)],
   Terminal),
  (804,
   [(804, Pc JUMPDEST), (805, Stack (PUSH_N [0])), (807, Dup 5), (808, Arith inst_EQ), (809, Arith ISZERO),
    (810, Stack (PUSH_N [3, 0x82]))],
   Jumpi),
  (814,
   [(814, Stack (PUSH_N [3, 0x7C])), (817, Dup 4), (818, Dup 4), (819, Dup 4), (820, Dup 4), (821, Dup 0), (822, Dup 0),
    (823, Stack (PUSH_N [0x1F])), (825, Arith ADD), (826, Stack (PUSH_N [0x20])), (828, Dup 0), (829, Swap 1), (830, Arith DIV),
    (831, Arith MUL), (832, Stack (PUSH_N [0x20])), (834, Arith ADD), (835, Stack (PUSH_N [0x40])), (837, Memory MLOAD), (838, Swap 0),
    (839, Dup 1), (840, Arith ADD), (841, Stack (PUSH_N [0x40])), (843, Memory MSTORE), (844, Dup 0), (845, Swap 3), (846, Swap 2),
    (847, Swap 1), (848, Swap 0), (849, Dup 1), (850, Dup 1), (851, Memory MSTORE), (852, Stack (PUSH_N [0x20])), (854, Arith ADD),
    (855, Dup 3), (856, Dup 3), (857, Dup 0), (858, Dup 2), (859, Dup 4), (860, Memory CALLDATACOPY), (861, Stack (PUSH_N [0])),
    (863, Dup 1), (864, Dup 4), (865, Arith ADD), (866, Memory MSTORE), (867, Stack (PUSH_N [0x1F])), (869, Bits inst_NOT),
    (870, Stack (PUSH_N [0x1F])), (872, Dup 2), (873, Arith ADD), (874, Bits inst_AND), (875, Swap 0), (876, Stack POP), (877, Dup 0),
    (878, Dup 3), (879, Arith ADD), (880, Swap 2), (881, Stack POP), (882, Stack POP), (883, Stack POP), (884, Stack POP),
    (885, Stack POP), (886, Stack POP), (887, Stack POP), (888, Stack (PUSH_N [6, 0xF0]))],
   Jump),
  (892, [(892, Pc JUMPDEST), (893, Stack POP), (894, Stack (PUSH_N [4, 0x2B]))], Jump),
  (898,
   [(898, Pc JUMPDEST), (899, Stack (PUSH_N [1])), (901, Dup 5), (902, Arith inst_EQ), (903, Arith ISZERO),
    (904, Stack (PUSH_N [4, 0x25]))],
   Jumpi),
  (908,
   [(908, Stack (PUSH_N [0])), (910, Stack (PUSH_N [3, 0xDA])), (913, Dup 3), (914, Dup 3), (915, Dup 0), (916, Dup 0),
    (917, Stack (PUSH_N [0x1F])), (919, Arith ADD), (920, Stack (PUSH_N [0x20])), (922, Dup 0), (923, Swap 1), (924, Arith DIV),
    (925, Arith MUL), (926, Stack (PUSH_N [0x20])), (928, Arith ADD), (929, Stack (PUSH_N [0x40])), (931, Memory MLOAD), (932, Swap 0),
    (933, Dup 1), (934, Arith ADD), (935, Stack (PUSH_N [0x40])), (937, Memory MSTORE), (938, Dup 0), (939, Swap 3), (940, Swap 2),
    (941, Swap 1), (942, Swap 0), (943, Dup 1), (944, Dup 1), (945, Memory MSTORE), (946, Stack (PUSH_N [0x20])), (948, Arith ADD),
    (949, Dup 3), (950, Dup 3), (951, Dup 0), (952, Dup 2), (953, Dup 4), (954, Memory CALLDATACOPY), (955, Stack (PUSH_N [0])),
    (957, Dup 1), (958, Dup 4), (959, Arith ADD), (960, Memory MSTORE), (961, Stack (PUSH_N [0x1F])), (963, Bits inst_NOT),
    (964, Stack (PUSH_N [0x1F])), (966, Dup 2), (967, Arith ADD), (968, Bits inst_AND), (969, Swap 0), (970, Stack POP), (971, Dup 0),
    (972, Dup 3), (973, Arith ADD), (974, Swap 2), (975, Stack POP), (976, Stack POP), (977, Stack POP), (978, Stack POP),
    (979, Stack POP), (980, Stack POP), (981, Stack POP), (982, Stack (PUSH_N [7, 8]))],
   Jump),
  (986,
   [(986, Pc JUMPDEST), (987, Swap 0), (988, Stack POP), (989, Dup 0),
    (990,
     Stack
      (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
    (1011, Bits inst_AND),
    (1012,
     Stack
      (PUSH_N
        [0xCF, 0x78, 0xCF, 0xD, 0x6F, 0x3D, 0x83, 0x71, 0xE1, 7, 0x5C, 0x69, 0xC4, 0x92, 0xAB, 0x4E, 0xC5, 0xD8, 0xCF, 0x23, 0xA1, 0xA2,
         0x39, 0xB6, 0xA5, 0x1A, 0x1D, 0, 0xBE, 0x7C, 0xA3, 0x12])),
    (1045, Stack (PUSH_N [0x40])), (1047, Memory MLOAD), (1048, Stack (PUSH_N [0x40])), (1050, Memory MLOAD), (1051, Dup 0),
    (1052, Swap 1), (1053, Arith SUB), (1054, Swap 0), (1055, Log LOG2), (1056, Stack POP), (1057, Stack (PUSH_N [4, 0x2A]))],
   Jump),
  (1061, [(1061, Pc JUMPDEST), (1062, Stack (PUSH_N [0])), (1064, Dup 0), (1065, Unknown 0xFD)], Terminal),
  (1066, [(1066, Pc JUMPDEST)], Next),
  (1067, [(1067, Pc JUMPDEST), (1068, Stack POP), (1069, Stack POP), (1070, Stack POP), (1071, Stack POP), (1072, Stack POP)], Jump),
  (1074,
   [(1074, Pc JUMPDEST), (1075, Stack (PUSH_N [0])), (1077, Dup 0), (1078, Stack (PUSH_N [0])), (1080, Dup 3), (1081, Dup 1),
    (1082, Memory MSTORE), (1083, Stack (PUSH_N [0x20])), (1085, Arith ADD), (1086, Swap 0), (1087, Dup 1), (1088, Memory MSTORE),
    (1089, Stack (PUSH_N [0x20])), (1091, Arith ADD), (1092, Stack (PUSH_N [0])), (1094, Arith SHA3), (1095, Storage SLOAD),
    (1096, Swap 0), (1097, Stack POP), (1098, Swap 1), (1099, Swap 0), (1100, Stack POP)],
   Jump),
  (1102,
   [(1102, Pc JUMPDEST), (1103, Stack (PUSH_N [1])), (1105, Stack (PUSH_N [0])), (1107, Swap 0), (1108, Storage SLOAD), (1109, Swap 0),
    (1110, Stack (PUSH_N [1, 0])), (1113, Arith EXP), (1114, Swap 0), (1115, Arith DIV),
    (1116,
     Stack
      (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
    (1137, Bits inst_AND),
    (1138,
     Stack
      (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
    (1159, Bits inst_AND), (1160, Info CALLER),
    (1161,
     Stack
      (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
    (1182, Bits inst_AND), (1183, Arith inst_EQ), (1184, Arith ISZERO), (1185, Arith ISZERO), (1186, Stack (PUSH_N [5, 0x13]))],
   Jumpi),
  (1190,
   [(1190, Stack (PUSH_N [0x40])), (1192, Memory MLOAD),
    (1193, Stack (PUSH_N [8, 0xC3, 0x79, 0xA0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])),
    (1226, Dup 1), (1227, Memory MSTORE), (1228, Stack (PUSH_N [4])), (1230, Arith ADD), (1231, Dup 0), (1232, Dup 0),
    (1233, Stack (PUSH_N [0x20])), (1235, Arith ADD), (1236, Dup 2), (1237, Dup 1), (1238, Arith SUB), (1239, Dup 2),
    (1240, Memory MSTORE), (1241, Stack (PUSH_N [0x12])), (1243, Dup 1), (1244, Memory MSTORE), (1245, Stack (PUSH_N [0x20])),
    (1247, Arith ADD), (1248, Dup 0),
    (1249,
     Stack
      (PUSH_N
        [0x6F, 0x6E, 0x6C, 0x79, 0x2D, 0x6F, 0x77, 0x6E, 0x65, 0x72, 0x2D, 0x61, 0x6C, 0x6C, 0x6F, 0x77, 0x65, 0x64, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0])),
    (1282, Dup 1), (1283, Memory MSTORE), (1284, Stack POP), (1285, Stack (PUSH_N [0x20])), (1287, Arith ADD), (1288, Swap 1),
    (1289, Stack POP), (1290, Stack POP), (1291, Stack (PUSH_N [0x40])), (1293, Memory MLOAD), (1294, Dup 0), (1295, Swap 1),
    (1296, Arith SUB), (1297, Swap 0), (1298, Unknown 0xFD)],
   Terminal),
  (1299,
   [(1299, Pc JUMPDEST), (1300, Dup 0), (1301, Stack (PUSH_N [0])), (1303, Dup 0), (1304, Dup 4), (1305, Dup 1), (1306, Memory MSTORE),
    (1307, Stack (PUSH_N [0x20])), (1309, Arith ADD), (1310, Swap 0), (1311, Dup 1), (1312, Memory MSTORE), (1313, Stack (PUSH_N [0x20])),
    (1315, Arith ADD), (1316, Stack (PUSH_N [0])), (1318, Arith SHA3), (1319, Dup 1), (1320, Swap 0), (1321, Storage SSTORE),
    (1322, Stack POP), (1323, Dup 0), (1324, Dup 2),
    (1325,
     Stack
      (PUSH_N
        [0x35, 0x55, 0x35, 0x80, 0xE4, 0x55, 0x3C, 0x90, 0x9A, 0xBE, 0xB5, 0x76, 0x4E, 0x84, 0x2C, 0xE1, 0xF9, 0x3C, 0x45, 0xF9, 0xF6,
         0x14, 0xBD, 0xE2, 0xA2, 0xCA, 0x5F, 0x5B, 0x7B, 0x7D, 0xC0, 0xFB])),
    (1358, Stack (PUSH_N [0x40])), (1360, Memory MLOAD), (1361, Stack (PUSH_N [0x40])), (1363, Memory MLOAD), (1364, Dup 0),
    (1365, Swap 1), (1366, Arith SUB), (1367, Swap 0), (1368, Log LOG3), (1369, Stack POP), (1370, Stack POP)],
   Jump),
  (1372,
   [(1372, Pc JUMPDEST), (1373, Stack (PUSH_N [1])), (1375, Stack (PUSH_N [0])), (1377, Swap 0), (1378, Storage SLOAD), (1379, Swap 0),
    (1380, Stack (PUSH_N [1, 0])), (1383, Arith EXP), (1384, Swap 0), (1385, Arith DIV),
    (1386,
     Stack
      (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
    (1407, Bits inst_AND), (1408, Dup 1)],
   Jump),
  (1410,
   [(1410, Pc JUMPDEST), (1411, Stack (PUSH_N [1])), (1413, Stack (PUSH_N [0])), (1415, Swap 0), (1416, Storage SLOAD), (1417, Swap 0),
    (1418, Stack (PUSH_N [1, 0])), (1421, Arith EXP), (1422, Swap 0), (1423, Arith DIV),
    (1424,
     Stack
      (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
    (1445, Bits inst_AND),
    (1446,
     Stack
      (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
    (1467, Bits inst_AND), (1468, Info CALLER),
    (1469,
     Stack
      (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
    (1490, Bits inst_AND), (1491, Arith inst_EQ), (1492, Arith ISZERO), (1493, Arith ISZERO), (1494, Stack (PUSH_N [6, 0x47]))],
   Jumpi),
  (1498,
   [(1498, Stack (PUSH_N [0x40])), (1500, Memory MLOAD),
    (1501, Stack (PUSH_N [8, 0xC3, 0x79, 0xA0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])),
    (1534, Dup 1), (1535, Memory MSTORE), (1536, Stack (PUSH_N [4])), (1538, Arith ADD), (1539, Dup 0), (1540, Dup 0),
    (1541, Stack (PUSH_N [0x20])), (1543, Arith ADD), (1544, Dup 2), (1545, Dup 1), (1546, Arith SUB), (1547, Dup 2),
    (1548, Memory MSTORE), (1549, Stack (PUSH_N [0x12])), (1551, Dup 1), (1552, Memory MSTORE), (1553, Stack (PUSH_N [0x20])),
    (1555, Arith ADD), (1556, Dup 0),
    (1557,
     Stack
      (PUSH_N
        [0x6F, 0x6E, 0x6C, 0x79, 0x2D, 0x6F, 0x77, 0x6E, 0x65, 0x72, 0x2D, 0x61, 0x6C, 0x6C, 0x6F, 0x77, 0x65, 0x64, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0])),
    (1590, Dup 1), (1591, Memory MSTORE), (1592, Stack POP), (1593, Stack (PUSH_N [0x20])), (1595, Arith ADD), (1596, Swap 1),
    (1597, Stack POP), (1598, Stack POP), (1599, Stack (PUSH_N [0x40])), (1601, Memory MLOAD), (1602, Dup 0), (1603, Swap 1),
    (1604, Arith SUB), (1605, Swap 0), (1606, Unknown 0xFD)],
   Terminal),
  (1607,
   [(1607, Pc JUMPDEST), (1608, Dup 0), (1609, Stack (PUSH_N [1])), (1611, Stack (PUSH_N [0])), (1613, Stack (PUSH_N [1, 0])),
    (1616, Arith EXP), (1617, Dup 1), (1618, Storage SLOAD), (1619, Dup 1),
    (1620,
     Stack
      (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
    (1641, Arith MUL), (1642, Bits inst_NOT), (1643, Bits inst_AND), (1644, Swap 0), (1645, Dup 3),
    (1646,
     Stack
      (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
    (1667, Bits inst_AND), (1668, Arith MUL), (1669, Bits inst_OR), (1670, Swap 0), (1671, Storage SSTORE), (1672, Stack POP),
    (1673, Stack (PUSH_N [1])), (1675, Stack (PUSH_N [0])), (1677, Swap 0), (1678, Storage SLOAD), (1679, Swap 0),
    (1680, Stack (PUSH_N [1, 0])), (1683, Arith EXP), (1684, Swap 0), (1685, Arith DIV),
    (1686,
     Stack
      (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
    (1707, Bits inst_AND),
    (1708,
     Stack
      (PUSH_N [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
    (1729, Bits inst_AND),
    (1730,
     Stack
      (PUSH_N
        [0xA2, 0xEA, 0x98, 0x83, 0xA3, 0x21, 0xA3, 0xE9, 0x7B, 0x82, 0x66, 0xC2, 0xB0, 0x78, 0xBF, 0xEE, 0xC6, 0xD5, 0xC, 0x71, 0x1E, 0xD7,
         0x1F, 0x87, 0x4A, 0x90, 0xD5, 0, 0xAE, 0x2E, 0xAF, 0x36])),
    (1763, Stack (PUSH_N [0x40])), (1765, Memory MLOAD), (1766, Stack (PUSH_N [0x40])), (1768, Memory MLOAD), (1769, Dup 0),
    (1770, Swap 1), (1771, Arith SUB), (1772, Swap 0), (1773, Log LOG2), (1774, Stack POP)],
   Jump),
  (1776,
   [(1776, Pc JUMPDEST), (1777, Stack (PUSH_N [0])), (1779, Dup 0), (1780, Stack (PUSH_N [0])), (1782, Dup 3), (1783, Memory MLOAD),
    (1784, Stack (PUSH_N [0x20])), (1786, Dup 5), (1787, Arith ADD), (1788, Dup 6), (1789, Dup 8), (1790, Info GAS), (1791, Misc CALL)],
   Terminal),
  (1792, [(1792, Swap 0), (1793, Stack POP), (1794, Swap 3), (1795, Swap 2), (1796, Stack POP), (1797, Stack POP), (1798, Stack POP)],
   Jump),
  (1800,
   [(1800, Pc JUMPDEST), (1801, Stack (PUSH_N [0])), (1803, Dup 1), (1804, Memory MLOAD), (1805, Stack (PUSH_N [0x20])), (1807, Dup 3),
    (1808, Arith ADD), (1809, Stack (PUSH_N [0])), (1811, Misc CREATE)],
   Terminal),
  (1812, [(1812, Swap 0), (1813, Stack POP), (1814, Swap 1), (1815, Swap 0), (1816, Stack POP)], Jump),
  (1818, [(1818, Unknown 0xFE)], Terminal),
  (1819, [(1819, Log LOG1), (1820, Stack (PUSH_N [0x62, 0x7A, 0x7A, 0x72, 0x30, 0x58])), (1827, Arith SHA3), (1828, Unknown 0xAC)],
   Terminal),
  (1829, [(1829, Unknown 0xB1)], Terminal), (1830, [(1830, Unknown 0xFC)], Terminal),
  (1831, [(1831, Arith ADD), (1832, Memory MSIZE), (1833, Stack CALLDATALOAD), (1834, Misc CREATE)], Terminal),
  (1835, [(1835, Unknown 0xD9)], Terminal),
  (1836,
   [(1836, Stack (PUSH_N [0x91, 0xC0, 0x2F, 0x91, 0x89, 0xF2, 0xE6, 8, 0x4E, 0x45, 0x6E, 0x20, 0x39, 0x4C])),
    (1851, Stack (PUSH_N [9, 0x66, 0xF, 0x41, 0xE, 0xF1, 0xC2, 0x5B, 0, 0x29]))],
   Next)]"
  by eval

definition changeOwner_hash :: "32 word"  where
 "changeOwner_hash = 0xa6f9dae1"

definition execute_hash :: "32 word"  where
 "execute_hash = 0x44c028fe"

definition getData_hash :: "32 word"  where
 "getData_hash = 0x54f6127f"

definition owner_hash :: "32 word"  where
 "owner_hash = 0x8da5cb5b"

definition setData_hash :: "32 word"  where
 "setData_hash = 0x749ebfb8"

lemma hash_diff:
  "ucast (hash::32 word) = (0xa6f9dae1::w256) \<Longrightarrow> hash = 0xa6f9dae1 "
  "ucast (hash::32 word) = (0x44c028fe::w256) \<Longrightarrow> hash = 0x44c028fe "
  "ucast (hash::32 word) = (0x54f6127f::w256) \<Longrightarrow> hash = 0x54f6127f "
  "ucast (hash::32 word) = (0x8da5cb5b::w256) \<Longrightarrow> hash = 0x8da5cb5b "
  "ucast (hash::32 word) = (0x749ebfb8::w256) \<Longrightarrow> hash = 0x749ebfb8 "
  by word_bitwise+

context
notes
  words_simps[simp add]
  calldataload_simps[simp add]
  M_def[simp add]
  Cmem_def[simp add]
  memory_range.simps[simp del]
 if_split[ split del ] sep_fun_simps[simp del]
gas_value_simps[simp add] gas_simps[simp] pure_emp_simps[simp add]
evm_fun_simps[simp add] sep_lc[simp del] sep_conj_first[simp add]
pure_false_simps[simp add] iszero_stack_def[simp add]
word256FromNat_def[simp add]
begin

method sep_imp_solve2 uses simp =
   solves \<open>rule conjI; rule refl\<close>
 | solves \<open>match conclusion in "block_lookup _ _ = Some _"  \<Rightarrow> \<open>simp add:word_rcat_simps\<close>
             , (rule conjI, (rule refl)+)\<close>
 | solves \<open>simp\<close>
 | solves \<open>(clarsimp?, order_sep_conj, ((((sep_cancel, clarsimp?)+)|simp add:simp|rule conjI)+)[1])\<close>
 | solves \<open>(clarsimp?, order_sep_conj, ((((sep_cancel, clarsimp?)+)|(clarsimp split:if_split simp: simp)|rule conjI)+)[1])\<close>
 | solves \<open>(clarsimp split:if_splits simp:word_rcat_simps) ; sep_imp_solve2 \<close>

method block_vcg2 uses simp=
  split_conds,
  ((blocks_rule_vcg; (rule refl)?), triple_seq_vcg),
  (sep_imp_solve2 simp:simp)+,
  (solves \<open>split_conds\<close>)?

definition
 store_mapping :: "address \<Rightarrow> w256"
 where
 "store_mapping addr \<equiv>  keccak (bytestr (w256 addr) @ bytestr (0::w256))"

method fast_sep_imp_solve uses simp =
  (match conclusion  in "triple_blocks _ _ _ _ _"  \<Rightarrow> \<open>succeed\<close> | 
    ( sep_imp_solve2 simp:simp, fast_sep_imp_solve simp: simp) )

method triple_blocks_vcg =
  (clarsimp simp only: sep_conj_ac(2)[symmetric])?,
  ((rule blocks_jumpi_uint_ex blocks_jump_uint_ex blocks_no_ex blocks_next_ex); 
   (clarsimp simp only: sep_conj_ac(2))?),
  triple_seq_vcg

abbreviation (input) owner_addr :: w256  where
  "owner_addr \<equiv> 0x1"

lemma word_or_one_neq_zero[simp]:
 "(a::'a::len word) || 1 \<noteq> 0"
  by (simp add: word_or_zero)

lemma memory_range_Nil:
 "(memory_range addr xs ** R) = (memory_range addr [] ** memory_range addr xs ** R)"
  by (case_tac xs; clarsimp simp: memory_range.simps)

lemma memory_range_w256_split_4_24:
 "(memory_range addr [b1,b2,b3,b4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0] ** R) =
  (memory_range addr [b1,b2,b3,b4] ** memory_range (addr+4) (replicate 28 (0::byte)) ** R)"
  by (simp add: memory_range.simps add.commute)

lemma memory_range_wrsplit_decomp:
 " memory_range addr (word_rsplit (v::256 word)) =
  (memory_range addr (take 4 (word_rsplit (v::256 word) :: byte list)) ** memory_range (addr+4) (drop 4 (word_rsplit (v::256 word) :: byte list)))"
  sorry

lemma rep28_eq_wrsplit:
 "replicate 28 (0::byte) = word_rsplit (0::224 word)"
  by (simp add:word_rsplit_def bin_rsplit_def bin_cat_def)

lemmas memory_range_w256_split_4_24_wrsplit =
  memory_range_w256_split_4_24[simplified rep28_eq_wrsplit]

definition
  input_data :: "32 word \<Rightarrow> w256 \<Rightarrow> address \<Rightarrow> w256 \<Rightarrow> byte list \<Rightarrow> byte list" 
where
 "input_data hash op_type to v xs \<equiv> bytestr hash @ (if hash = execute_hash then bytestr op_type @ bytestr (w256 to) @ bytestr v @ bytestr ((of_nat (length xs)) :: w256)  @ xs else xs)"

lemma input_data_extract_hash:
 "input_data hash op_type to v xs = bytestr hash @ drop 4 (input_data hash op_type to v xs)"
  by (simp add: input_data_def len_bytestr_simps)

lemma length_input_data_ge_4[simp]:
 "length (input_data hash op_type to v xs) \<ge> 4"
  by (subst input_data_extract_hash)
     (simp add: len_bytestr_simps)

lemma ucast_sym:
 "UCAST(32 \<rightarrow> 256) w = v \<Longrightarrow> w = UCAST (256 \<rightarrow> 32) v"
  by (drule sym)
     (simp add: is_up ucast_up_ucast_id)

lemma len_input_data_execute_hash:
 "length (input_data execute_hash op_type to v xs) \<ge> 132"
  by (simp add: input_data_def len_bytestr_simps)

lemma ucast_to_input_data[simplified execute_hash_def, simp]:
 "UCAST(256 \<rightarrow> 160) (word_rcat (take 32 
    (drop 36 (input_data execute_hash op_type to v xs)))
     && 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF) = to"
  apply(clarsimp simp: execute_hash_def input_data_def word_rcat_simps len_bytestr_simps)
  apply (clarsimp simp: bytestr_def)
  apply (subst word_rcat_rsplit)
  apply (simp add: w256_def)
  apply (subst ucast_and_w256_drop)
  apply (simp add: ucast_up_ucast_id is_up)
  done

lemma to_input_data_ucast[simplified execute_hash_def, simp]:
 "word_rcat (take 32 (drop 36 (input_data execute_hash op_type to v xs))) &&
           (0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF::w256) = UCAST(160\<rightarrow>256) to"
  apply(clarsimp simp: execute_hash_def input_data_def word_rcat_simps len_bytestr_simps)
  apply (clarsimp simp: bytestr_def)
  apply (subst word_rcat_rsplit)
  apply (simp add: w256_def)
  apply (subst ucast_and_w256_drop)
  apply (simp)
  done

lemma input_data_manip_is_hash:
  "word_of_int
     (uint
       (word_rcat
         (if 32 \<le> length (input_data hash op_type to v xs) then take 32 (input_data hash op_type to v xs)
          else let filling_len = 32 - length (take 32 (input_data hash op_type to v xs))
               in take 32 (input_data hash op_type to v xs) @ replicate filling_len 0) :: w256) div
      0x100000000000000000000000000000000000000000000000000000000) =
    UCAST(32 \<rightarrow> 256) hash"
  apply (subst  uint_div[where y="0x100000000000000000000000000000000000000000000000000000000::w256", symmetric, simplified])
  apply (clarsimp split: if_splits)
  apply (rule conjI; clarsimp)
   apply (subst input_data_extract_hash)
   apply (simp add: len_bytestr_simps)
   apply (simp only: bytestr_def)
   apply (subst  word_rcat_word_rsplit_div_rep0)
    apply simp
   apply (simp add: word_rcat_rsplit_ucast)
  apply (subst input_data_extract_hash)
  apply simp
  apply (simp add: bytestr_def)
  apply ( subst  word_rcat_word_rsplit_div_rep0)
   apply simp
  apply (rule word_rcat_rsplit_ucast)
  done

lemma drop_100_input_data[simplified execute_hash_def]:
 "drop 100 (input_data execute_hash op_type to v xs) = bytestr (of_nat (length xs)::w256) @ xs"
  by (simp add: input_data_def len_bytestr_simps)

lemma len_xs_of_input_data[simplified execute_hash_def, simp]:
 "(word_rcat (take 32 (drop 100 (input_data execute_hash op_type to v xs)))::w256) = of_nat (length xs)"
  by (simp add: execute_hash_def input_data_def  bytestr_def word_rcat_rsplit)

lemma op_type_of_input_data[simplified execute_hash_def, simp]:
 "word_rcat (take 32 (drop 4 (input_data execute_hash op_type to v xs))) = op_type"
  by (simp add: input_data_def execute_hash_def   bytestr_def word_rcat_rsplit)


lemma to_of_input_data[simplified execute_hash_def, simp]:
 "word_rcat (take 32 (drop 36 (input_data execute_hash op_type to v xs))) = UCAST(160\<rightarrow>256) to"
  by (simp add: input_data_def execute_hash_def   bytestr_def word_rcat_rsplit w256_def)

lemma v_in_input_data[simplified execute_hash_def, simp]:
 "word_rcat (take 32 (drop 68 (input_data execute_hash op_type to v xs))) = v"
  by (clarsimp simp: execute_hash_def input_data_def word_rcat_simps len_bytestr_simps bytestr_def word_rcat_rsplit)

lemma length_input_data_cond[simplified execute_hash_def]:
 "length xs \<le> 0x100000000 \<Longrightarrow>
  length (input_data execute_hash op_type to v xs) > unat ((4::w256) + word_rcat (take 32 (drop 100 (input_data execute_hash op_type to v xs))))"
  apply (simp add: input_data_def len_bytestr_simps bytestr_def word_rcat_rsplit execute_hash_def)
  apply (simp add: unat_ucast_less_no_overflow_simp  word_add_less_mono1 le_unat_uoi[where z="2^64"])+
  done

lemma length_input_data_gt_len_xs[simplified execute_hash_def, simp]:
 "length xs \<le> 0x100000000 \<Longrightarrow>
  length (input_data execute_hash op_type to v xs) > unat ((4::w256) + of_nat (length xs))"
  apply (simp add: execute_hash_def input_data_def len_bytestr_simps)
  apply (simp add: unat_ucast_less_no_overflow_simp  word_add_less_mono1 le_unat_uoi[where z="2^64"])
  done

theorem verify_identity_return:
  notes
    bit_mask_rev[simp]
  address_mask_ucast[simp] address_mask_ucast[simplified word_bool_alg.conj.commute, simp]
  ucast_and_w256_drop[simp]
  word_bool_alg.conj.commute[simp]
  length_word_rsplit_4[simp]
  ucast_160_upto_256_eq[simp]
  hash_diff[simp]
  eval_bit_mask[simp]
  len_bytestr_simps[simp]
fixes hash::"32 word"
  and op_type::"w256"
  and to ::"address"
assumes blk_num: "bn > 2463000"
    and net: "at_least_eip150 net"
    and woi_input_data_ge_4: "word_of_int (int (length (input_data hash op_type to v xs))) \<ge> (4::w256)"
    and xs: "length xs \<le> 0x100000000" 
  shows
"\<exists>r. bbtriple net
  (\<langle>ucast owner \<noteq> sender \<and> addr \<noteq> 1\<rangle> **
   program_counter 0 ** stack_height 0 **
   sent_data (input_data hash op_type to v xs) **
   sent_value 0 ** caller sender ** blk_num bn **
   memory_usage 0 ** continuing ** gas_pred 100000 **
   account_existence to to_ex  **
   account_existence sender sender_ex  **
   account_existence 0 False **
   storage owner_addr owner **
   storage addr val **
   memory 0x00 m0x0 **
   memory 0x20 m0x20 **
   memory 0x40 (bytestr_to_w256 [x]) **
   memory 0x60 (bytestr_to_w256 [y]) **
   memory_range 0x80 (word_rsplit (m0x80::w256)) **
   memory_range 0x84 (word_rsplit (m0x84::w256)) **
   memory_range 0xA0 (word_rsplit (wdata_sz::w256)) **
   memory_range 0xA1 (word_rsplit (wdata::w256)) **
   memory 0xA4 m0xA4 **
   memory 0xC4 m0xC4 **
   log_number log_num **
   this_account this)
  blocks_identity
  (storage owner_addr owner ** storage addr val ** r)"
  apply (insert blk_num[simplified word_less_nat_alt] net)
  apply (simp add: Let_def)
  apply (simp add:blocks_identity_simp)
  apply(simp add: blocks_simps bbtriple_def )
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (simp add: word_rcat_simps)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply split_conds
  using woi_input_data_ge_4 apply (simp)
  apply split_conds
  apply (triple_blocks_vcg)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
  apply (simp add: word_rcat_simps)
  apply split_conds
   apply split_conds

   apply (simp add: input_data_manip_is_hash)
  apply (triple_blocks_vcg)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
  apply (triple_blocks_vcg)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2 simp: word_rcat_simps)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
  apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
   apply split_conds
   apply split_conds
      apply split_conds
       apply (drule ucast_sym)
  apply simp
  using len_input_data_execute_hash[simplified execute_hash_def, of op_type to v xs]
       apply fastforce
  apply (drule ucast_sym)
        apply split_conds
   apply split_conds
    apply (triple_blocks_vcg)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
    apply (sep_imp_solve2 simp: word_rcat_simps)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2 simp: word_rcat_simps)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2 simp: word_rcat_simps)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
   apply split_conds
   apply split_conds
         apply split_conds
(* HERE *)
          apply (simp add: input_data_def execute_hash_def)
  using length_input_data_cond[OF xs, of op_type to v ]
  apply simp
  apply (triple_blocks_vcg)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
   apply split_conds
   apply split_conds
   apply (triple_blocks_vcg)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
   apply split_conds
   apply split_conds
   apply (triple_blocks_vcg)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2 simp: word_rcat_simps)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2 simp: word_rcat_simps)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2 simp: word_rcat_simps)
  apply (simp add: word_rcat_simps)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))+
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
   apply (sep_imp_solve2)
    apply (sep_imp_solve2 (* simp: memory_def[ where ind="0x80"] *))
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
  apply (simp add: word_rcat_simps)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0xA1"])?))+
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
  apply (triple_blocks_vcg)
    apply (sep_imp_solve2 simp: word_rcat_simps)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
  apply (simp add: word_rcat_simps)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
                   apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))+
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
   apply (subst stack_topmost3_unfold_sep)
   apply (subst (asm) memory_range_Nil[where addr="0xA0"])
   apply (sep_imp_solve2 simp:  word_rcat_simps memory_def[ where ind="0xA0"])
    apply (sep_imp_solve2)
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2 simp: word_rcat_simps)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2 simp: word_rcat_simps)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
  apply (simp add: word_rcat_simps)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
                   apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))+
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0xA0"])?))+
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
  apply (triple_blocks_vcg)
    apply (sep_imp_solve2 simp: word_rcat_simps)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
  apply (simp add: word_rcat_simps)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
                   apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))+
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (subst stack_topmost_unfold_sep')
  apply (drule ucast_sym)
  using len_input_data_execute_hash[of op_type to v xs, simplified execute_hash_def]
  using ucast_to_input_data[of op_type to v xs, simplified execute_hash_def]
            apply (clarsimp simp:  word_rcat_simps )
  using len_input_data_execute_hash[of op_type to v xs, simplified execute_hash_def]
  using ucast_to_input_data[of op_type to v xs, simplified execute_hash_def]
            apply (clarsimp simp:  word_rcat_simps )
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
   apply split_conds
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2 simp: word_rcat_simps)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
    apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x84"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x84"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x84"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x84"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x84"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x84"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x84"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x84"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0xA0"])?))
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x84"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x84"])?))
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x84"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x84"])?))
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)

  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (simp add: word_rcat_simps)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (simp add: word_rcat_simps)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
                   apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))+
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)

  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2 simp: word_rcat_simps memory_def[ where ind="0x80"])
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2 simp: word_rcat_simps)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
   apply split_conds
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
   apply split_conds
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
   apply split_conds
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
   apply split_conds
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))+
 apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0xA1"])?))+
  apply (sep_imp_solve2 simp: word_rcat_simps)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))+
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (subst stack_topmost3_unfold_sep)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
   apply (subst (asm) memory_range_Nil[where addr="0xA0"])
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"] memory_def[ where ind="0x84"])?))

            apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
   apply split_conds
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))+

  apply (sep_imp_solve2 simp: word_rcat_simps memory_def[where ind="0x80"])
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0xA0"])?))+

  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))+

  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
           apply (sep_simp simp: stack_topmost_unfold_sep')
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0xA0"])?))
  apply (subst (asm) memory_range_Nil[where addr="0xA0"])
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps )?))
  apply (clarsimp simp add: Ccallgas_def calc_memu_extra_def L_def split: if_splits)+
  apply (sep_imp_solve2)
   apply split_conds
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (clarsimp simp: word_rcat_simps)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
  apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))+
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
 apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x84"])?))+
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
 apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (drule ucast_sym)
  using len_input_data_execute_hash[of op_type to v xs, simplified execute_hash_def]
  apply simp
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
   apply split_conds
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
   apply split_conds
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2 simp: word_rcat_simps)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2 simp: word_rcat_simps)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2 simp: word_rcat_simps)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2 simp: word_rcat_simps)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2 simp: word_rcat_simps)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
   apply split_conds
   apply split_conds
   apply split_conds
   apply split_conds
   apply split_conds
   apply split_conds
   apply split_conds
   apply split_conds
   apply split_conds

  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
   apply split_conds
   apply split_conds
   apply split_conds
   apply split_conds
   apply split_conds
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2 simp: word_rcat_simps)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2 simp: word_rcat_simps)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))+
  apply (sep_imp_solve2 simp: word_rcat_simps)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0xA0"])?))+
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))+
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
              apply (sep_imp_solve2)
  apply (subst stack_topmost3_unfold_sep)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply (subst (asm) memory_range_Nil[where addr="0xA0"])
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
   apply split_conds
   apply split_conds
   apply split_conds
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)

  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)

  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))
   apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)

  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0xA0"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0xA0"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0xA0"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0xA0"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0xA0"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0xA0"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0xA0"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0xA0"])?))
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0xA0"])?))
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
            apply (sep_imp_solve2)
  apply (drule ucast_sym)
  using length_input_data_cond[OF xs, of op_type to v]
           apply simp
  apply (drule ucast_sym)
  using length_input_data_cond[OF xs, of op_type to v]
           apply simp
  apply split_conds
  apply split_conds
          apply split_conds
  apply (drule ucast_sym)
  apply simp
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))+
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2 simp: word_rcat_simps)
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x84"])?))+
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2 simp: word_rcat_simps)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply split_conds
  apply split_conds
  apply split_conds
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply split_conds
  apply split_conds
  apply split_conds
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)

  apply split_conds
  apply split_conds
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply split_conds
  apply split_conds
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2 simp: word_rcat_simps)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))+
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
                      apply ((sep_cancel, (clarsimp simp: word_rcat_simps memory_def[ where ind="0x80"])?))+
  apply (simp add: word_rcat_rsplit  bytestr_def)
  oops
  find_theorems word_rcat word_rsplit
  thm word_rcat_rsplit[simplified bytestr_def[symmetric]] bytestr_def
lemma
"word_rcat
            (take 32
              (drop (unat ((4::w256) + word_rcat (bytestr ((of_nat (length xs))::w256))))
                (input_data 0x44C028FE op_type to v xs))) = undefined"
  apply (simp add: input_data_def execute_hash_def bytestr_def word_rcat_rsplit)
  using [[show_types]]
  oops
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)

                      
                      
                      apply (subst stack_topmost_unfold_sep')
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
             apply ((sep_cancel, (clarsimp simp: word_rcat_simps ucast_to_input_data memory_def[ where ind="0x80"])?))
             apply ((sep_cancel, (clarsimp simp: word_rcat_simps ucast_to_input_data memory_def[ where ind="0x80"])?))
             apply ((sep_cancel, (clarsimp simp: word_rcat_simps ucast_to_input_data memory_def[ where ind="0x80"])?))
             apply ((sep_cancel, (clarsimp simp: word_rcat_simps ucast_to_input_data memory_def[ where ind="0x80"])?))
             apply ((sep_cancel, (clarsimp simp: word_rcat_simps ucast_to_input_data memory_def[ where ind="0x80"])?))
             apply ((sep_cancel, (clarsimp simp: word_rcat_simps ucast_to_input_data memory_def[ where ind="0x80"])?))
             apply ((sep_cancel, (clarsimp simp: word_rcat_simps ucast_to_input_data memory_def[ where ind="0x80"])?))
             apply ((sep_cancel, (clarsimp simp: word_rcat_simps ucast_to_input_data memory_def[ where ind="0x80"])?))
             apply ((sep_cancel, (clarsimp simp: word_rcat_simps ucast_to_input_data memory_def[ where ind="0x80"])?))
  apply (drule ucast_sym)
  apply simp
  apply (subst ucast_to_input_data[simplified execute_hash_def])
  apply (subst (asm) memory_range_Nil[where addr="0xA0"])
  apply (sep_imp_solve2)
  apply (drule ucast_sym)
  apply simp
            apply (clarsimp simp: to_input_data_ucast[simplified execute_hash_def])
  apply (subgoal_tac "word_rcat (take 32 (drop 68 (input_data 0x44C028FE op_type to v xs))) = v")
             apply (clarsimp simp: Ccallgas_def calc_memu_extra_def L_def split: if_splits)
  defer
  apply (sep_imp_solve2)
   apply split_conds
   apply split_conds
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
             apply ((sep_cancel, (clarsimp simp: word_rcat_simps ucast_to_input_data memory_def[ where ind="0x80"])?))
             apply ((sep_cancel, (clarsimp simp: word_rcat_simps ucast_to_input_data memory_def[ where ind="0x80"])?))
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (clarsimp?, order_sep_conj)
              apply (((sep_cancel, clarsimp?)+)|simp add:|rule conjI)
             apply ((sep_cancel, (clarsimp simp: word_rcat_simps ucast_to_input_data memory_def[ where ind="0x84"])?))+
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)

   apply split_conds
   apply split_conds
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
 
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)

  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2 simp: word_rcat_simps)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)

  apply split_conds
   apply split_conds
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply split_conds
   apply split_conds
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply split_conds
   apply split_conds
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply split_conds
   apply split_conds
   apply split_conds
   apply split_conds
  apply (triple_blocks_vcg)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply (sep_imp_solve2)
  apply -
  using [[show_types]] ucast_to_input_data[simplified execute_hash_def]
  apply -
  oops


end
